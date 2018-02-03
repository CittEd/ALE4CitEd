/*
 * Copyright (C) 2017 Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package it.cnr.istc.ale.server;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import it.cnr.istc.ale.api.Lesson;
import it.cnr.istc.ale.api.LessonAPI;
import it.cnr.istc.ale.api.messages.Event;
import it.cnr.istc.ale.api.messages.TokenUpdate;
import it.cnr.istc.ale.api.messages.HideEvent;
import it.cnr.istc.ale.api.messages.LostLesson;
import it.cnr.istc.ale.api.messages.Token;
import it.cnr.istc.ale.api.messages.NewLesson;
import it.cnr.istc.ale.api.messages.QuestionEvent;
import it.cnr.istc.ale.api.messages.TextEvent;
import it.cnr.istc.ale.api.model.LessonModel;
import it.cnr.istc.ale.api.model.QuestionEventTemplate;
import it.cnr.istc.ale.api.model.TextEventTemplate;
import static it.cnr.istc.ale.server.Context.EMF;
import static it.cnr.istc.ale.server.Context.MAPPER;
import it.cnr.istc.ale.server.db.LessonEntity;
import it.cnr.istc.ale.server.db.LessonModelEntity;
import it.cnr.istc.ale.server.db.RoleEntity;
import it.cnr.istc.ale.server.db.UserEntity;
import it.cnr.istc.ale.server.solver.LessonManager;
import it.cnr.istc.ale.server.solver.LessonManagerListener;
import it.cnr.istc.ale.server.solver.SolverToken;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import javax.persistence.EntityManager;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import org.eclipse.paho.client.mqttv3.MqttException;

/**
 *
 * @author Riccardo De Benedictis
 */
@Path("lessons")
public class LessonResource implements LessonAPI {

    private static final Logger LOG = Logger.getLogger(LessonResource.class.getName());

    @Override
    @POST
    @Path("new_lesson")
    @Produces(MediaType.APPLICATION_JSON)
    public Lesson new_lesson(@FormParam("teacher_id") long teacher_id, @FormParam("lesson_name") String lesson_name, @FormParam("model") String model, @FormParam("roles") String roles) {
        try {
            LessonModel lesson_model = MAPPER.readValue(model, LessonModel.class);
            Map<String, Long> lesson_roles = MAPPER.readValue(roles, new TypeReference<Map<String, Long>>() {
            });

            EntityManager em = EMF.createEntityManager();
            em.getTransaction().begin();
            LessonModelEntity lme = new LessonModelEntity();
            lme.setModel(model);
            em.persist(lme);
            UserEntity teacher = em.find(UserEntity.class, teacher_id);
            teacher.addModel(lme);
            LessonEntity le = new LessonEntity();
            le.setName(lesson_name);
            le.setTeacher(teacher);
            teacher.addLesson(le);
            for (Map.Entry<String, Long> role : lesson_roles.entrySet()) {
                UserEntity student = em.find(UserEntity.class, role.getValue());
                RoleEntity re = new RoleEntity();
                re.setStudent(student);
                re.setLesson(le);
                re.setName(role.getKey());
                student.addRole(re);
                em.persist(re);
                em.persist(student);
            }
            em.persist(le);
            teacher.addLesson(le);
            em.persist(teacher);
            em.getTransaction().commit();

            Lesson l = new Lesson(le.getId(), teacher_id, lesson_name, lesson_roles);
            LessonManager lm = new LessonManager();
            lm.addSolverListener(new LessonManagerListener() {
                @Override
                public void newToken(SolverToken tk) {
                    try {
                        // we notify the teacher that a new event has been created..
                        Context.getContext().mqtt.publish(teacher_id + "/input", MAPPER.writeValueAsBytes(new Token(l.getId(), tk.tp, tk.cause != null ? tk.cause.tp : null, (long) lm.network.getValue(tk.tp), tk.template.getName())), 1, false);
                    } catch (JsonProcessingException | MqttException ex) {
                        LOG.log(Level.SEVERE, null, ex);
                    }
                }

                @Override
                public void movedToken(SolverToken tk) {
                    try {
                        // we notify the teacher that an event has been updated..
                        Context.getContext().mqtt.publish(teacher_id + "/input", MAPPER.writeValueAsBytes(new TokenUpdate(l.getId(), tk.tp, (long) lm.network.getValue(tk.tp))), 1, false);
                    } catch (JsonProcessingException | MqttException ex) {
                        LOG.log(Level.SEVERE, null, ex);
                    }
                }

                @Override
                public void executeToken(SolverToken tk) {
                    try {
                        byte[] execute_event_bytes = null;
                        if (tk.template instanceof TextEventTemplate) {
                            execute_event_bytes = MAPPER.writeValueAsBytes(new TextEvent(l.getId(), tk.tp, ((TextEventTemplate) tk.template).getContent()));
                        } else if (tk.template instanceof QuestionEventTemplate) {
                            Collection<String> answers = new ArrayList<>(((QuestionEventTemplate) tk.template).getAnswers().size());
                            for (QuestionEventTemplate.Answer answer : ((QuestionEventTemplate) tk.template).getAnswers()) {
                                answers.add(MAPPER.writeValueAsString(answer));
                            }
                            execute_event_bytes = MAPPER.writeValueAsBytes(new QuestionEvent(l.getId(), tk.tp, ((QuestionEventTemplate) tk.template).getQuestion(), answers));
                        } else {
                            throw new UnsupportedOperationException("Not supported yet.");
                        }
                        try {
                            // we notify the teacher that a token has to be executed..
                            Context.getContext().mqtt.publish(teacher_id + "/input", execute_event_bytes, 1, false);
                        } catch (MqttException ex) {
                            LOG.log(Level.SEVERE, null, ex);
                        }
                        try {
                            // we notify the student associated to the token's role that a token has to be executed..
                            Context.getContext().mqtt.publish(lesson_roles.get(tk.template.getRole()) + "/input", execute_event_bytes, 1, false);
                        } catch (MqttException ex) {
                            LOG.log(Level.SEVERE, null, ex);
                        }
                    } catch (JsonProcessingException ex) {
                        LOG.log(Level.SEVERE, null, ex);
                    }
                }

                @Override
                public void hideToken(SolverToken tk) {
                    try {
                        byte[] hide_event_bytes = MAPPER.writeValueAsBytes(new HideEvent(l.getId(), tk.tp));
                        try {
                            // we notify the teacher that a token has to be hidden..
                            Context.getContext().mqtt.publish(teacher_id + "/input", hide_event_bytes, 1, false);
                        } catch (MqttException ex) {
                            LOG.log(Level.SEVERE, null, ex);
                        }
                        try {
                            // we notify the student associated to the token's role that a token has to be hidden..
                            Context.getContext().mqtt.publish(lesson_roles.get(tk.template.getRole()) + "/input", hide_event_bytes, 1, false);
                        } catch (MqttException ex) {
                            LOG.log(Level.SEVERE, null, ex);
                        }
                    } catch (JsonProcessingException ex) {
                        LOG.log(Level.SEVERE, null, ex);
                    }
                }

                @Override
                public void newTime(long time) {
                    try {
                        Context.getContext().mqtt.publish(teacher_id + "/input/lesson-" + l.getId() + "/time", Long.toString(time).getBytes(), 1, true);
                    } catch (MqttException ex) {
                        LOG.log(Level.SEVERE, null, ex);
                    }
                }
            });
            Context.getContext().lessons_lock.lock();
            try {
                Context.getContext().lessons.put(l.getId(), new LessonContext(l, lesson_model, lm));
            } finally {
                Context.getContext().lessons_lock.unlock();
            }

            // we notify all the students that a new lesson has been created..
            for (Long student_id : lesson_roles.values()) {
                try {
                    Context.getContext().mqtt.publish(student_id + "/input", MAPPER.writeValueAsBytes(new NewLesson(l)), 1, false);
                } catch (JsonProcessingException | MqttException ex) {
                    LOG.log(Level.SEVERE, null, ex);
                }
            }
            return l;
        } catch (IOException ex) {
            LOG.log(Level.SEVERE, null, ex);
            return null;
        }
    }

    @Override
    @PUT
    @Path("remove_lesson")
    public void remove_lesson(long lesson_id) {
        EntityManager em = EMF.createEntityManager();
        LessonEntity lesson = em.find(LessonEntity.class, lesson_id);
        em.getTransaction().begin();
        for (RoleEntity role : lesson.getRoles()) {
            em.remove(role);
            role.getStudent().removeRole(role);
            try {
                Context.getContext().mqtt.publish(role.getId() + "/input", MAPPER.writeValueAsBytes(new LostLesson(lesson_id)), 1, false);
            } catch (JsonProcessingException | MqttException ex) {
                LOG.log(Level.SEVERE, null, ex);
            }
        }
        em.remove(lesson);
        em.getTransaction().commit();
        Context.getContext().lessons_lock.lock();
        try {
            Context.getContext().lessons.remove(lesson_id);
        } finally {
            Context.getContext().lessons_lock.unlock();
        }
    }

    @Override
    @PUT
    @Path("solve_lesson")
    public void solve_lesson(long lesson_id) {
        Context.getContext().lessons_lock.lock();
        try {
            Context.getContext().lessons.get(lesson_id).getManager().setModel(Context.getContext().lessons.get(lesson_id).getModel());
        } finally {
            Context.getContext().lessons_lock.unlock();
        }
    }

    @Override
    @GET
    @Path("get_models")
    @Produces(MediaType.APPLICATION_JSON)
    public Collection<LessonModel> get_models(@QueryParam("teacher_id") long teacher_id) {
        EntityManager em = EMF.createEntityManager();
        UserEntity teacher = em.find(UserEntity.class, teacher_id);
        Collection<LessonModel> models = new ArrayList<>(teacher.getModels().size());
        for (LessonModelEntity model : teacher.getModels()) {
            try {
                models.add(MAPPER.readValue(model.getModel(), LessonModel.class));
            } catch (IOException ex) {
                LOG.log(Level.SEVERE, null, ex);
            }
        }
        return models;
    }

    @Override
    @GET
    @Path("get_lessons")
    @Produces(MediaType.APPLICATION_JSON)
    public Collection<Lesson> get_lessons(@QueryParam("teacher_id") long teacher_id) {
        EntityManager em = EMF.createEntityManager();
        UserEntity teacher = em.find(UserEntity.class, teacher_id);
        Collection<Lesson> lessons = new ArrayList<>(teacher.getLessons().size());
        for (LessonEntity lesson : teacher.getLessons()) {
            Map<String, Long> roles = new HashMap<>();
            for (RoleEntity role : lesson.getRoles()) {
                roles.put(role.getName(), role.getStudent().getId());
            }
            lessons.add(new Lesson(lesson.getId(), teacher_id, lesson.getName(), roles));
        }
        return lessons;
    }

    @Override
    @GET
    @Path("get_tokens")
    @Produces(MediaType.APPLICATION_JSON)
    public Collection<Token> get_tokens(@QueryParam("lesson_id") long lesson_id) {
        Context.getContext().lessons_lock.lock();
        try {
            return Context.getContext().lessons.get(lesson_id).getManager().getTokens().stream().map(tk -> new Token(lesson_id, tk.tp, tk.cause != null ? tk.cause.tp : null, (long) Context.getContext().lessons.get(lesson_id).getManager().network.getValue(tk.tp), tk.template.getName())).collect(Collectors.toList());
        } finally {
            Context.getContext().lessons_lock.unlock();
        }
    }

    @Override
    @GET
    @Path("get_followed_lessons")
    @Produces(MediaType.APPLICATION_JSON)
    public Collection<Lesson> get_followed_lessons(@QueryParam("student_id") long student_id) {
        EntityManager em = EMF.createEntityManager();
        UserEntity student = em.find(UserEntity.class, student_id);
        Collection<Lesson> lessons = new ArrayList<>(student.getLessons().size());
        for (RoleEntity role : student.getRoles()) {
            Map<String, Long> roles = new HashMap<>();
            for (RoleEntity r : role.getLesson().getRoles()) {
                roles.put(r.getName(), r.getStudent().getId());
            }
            lessons.add(new Lesson(role.getLesson().getId(), role.getLesson().getId(), role.getLesson().getName(), roles));
        }
        return lessons;
    }

    @Override
    @GET
    @Path("get_events")
    @Produces(MediaType.APPLICATION_JSON)
    public Collection<Event> get_events(@QueryParam("lesson_id") long lesson_id) {
        Context.getContext().lessons_lock.lock();
        try {
            return Context.getContext().lessons.get(lesson_id).getManager().getTokensUpToNow().stream().map(tk -> {
                if (tk.template instanceof TextEventTemplate) {
                    return new TextEvent(lesson_id, tk.tp, ((TextEventTemplate) tk.template).getContent());
                } else if (tk.template instanceof QuestionEventTemplate) {
                    Collection<String> answers = new ArrayList<>(((QuestionEventTemplate) tk.template).getAnswers().size());
                    for (QuestionEventTemplate.Answer answer : ((QuestionEventTemplate) tk.template).getAnswers()) {
                        try {
                            answers.add(MAPPER.writeValueAsString(answer));
                        } catch (JsonProcessingException ex) {
                            LOG.log(Level.SEVERE, null, ex);
                        }
                    }
                    return new QuestionEvent(lesson_id, tk.tp, ((QuestionEventTemplate) tk.template).getQuestion(), answers);
                } else {
                    throw new UnsupportedOperationException("Not supported yet.");
                }
            }).collect(Collectors.toList());
        } finally {
            Context.getContext().lessons_lock.unlock();
        }
    }

    @Override
    @PUT
    @Path("start_lesson")
    public void start_lesson(@FormParam("lesson_id") long lesson_id) {
        Context.getContext().lessons_lock.lock();
        try {
            Context.getContext().lessons.get(lesson_id).setRunning(true);
        } finally {
            Context.getContext().lessons_lock.unlock();
        }
    }

    @Override
    @PUT
    @Path("pause_lesson")
    public void pause_lesson(@FormParam("lesson_id") long lesson_id) {
        Context.getContext().lessons_lock.lock();
        try {
            Context.getContext().lessons.get(lesson_id).setRunning(false);
        } finally {
            Context.getContext().lessons_lock.unlock();
        }
    }

    @Override
    @PUT
    @Path("stop_lesson")
    public void stop_lesson(@FormParam("lesson_id") long lesson_id) {
        Context.getContext().lessons_lock.lock();
        try {
            Context.getContext().lessons.get(lesson_id).setRunning(false);
            Context.getContext().lessons.get(lesson_id).getManager().goTo(0);
        } finally {
            Context.getContext().lessons_lock.unlock();
        }
    }

    @Override
    @PUT
    @Path("go_at")
    public void go_at(@FormParam("lesson_id") long lesson_id, @FormParam("timestamp") long timestamp) {
        Context.getContext().lessons_lock.lock();
        try {
            Context.getContext().lessons.get(lesson_id).getManager().goTo(0);
        } finally {
            Context.getContext().lessons_lock.unlock();
        }
    }
}
