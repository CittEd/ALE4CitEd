/*
 * Copyright (C) 2018 Riccardo De Benedictis
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
package it.cnr.istc.lecture.webapp;

import it.cnr.istc.lecture.api.Credentials;
import it.cnr.istc.lecture.api.InitResponse;
import it.cnr.istc.lecture.api.Lesson;
import it.cnr.istc.lecture.api.NewLessonRequest;
import it.cnr.istc.lecture.api.NewUserRequest;
import it.cnr.istc.lecture.api.User;
import it.cnr.istc.lecture.api.messages.Event;
import it.cnr.istc.lecture.api.model.LessonModel;
import it.cnr.istc.lecture.webapp.entities.LessonEntity;
import it.cnr.istc.lecture.webapp.entities.LessonModelEntity;
import it.cnr.istc.lecture.webapp.entities.RoleEntity;
import it.cnr.istc.lecture.webapp.entities.UserEntity;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import javax.annotation.Resource;
import javax.ejb.EJB;
import javax.json.bind.Jsonb;
import javax.json.bind.JsonbBuilder;
import javax.persistence.EntityManager;
import javax.persistence.NoResultException;
import javax.persistence.PersistenceContext;
import javax.persistence.TypedQuery;
import javax.transaction.HeuristicMixedException;
import javax.transaction.HeuristicRollbackException;
import javax.transaction.NotSupportedException;
import javax.transaction.RollbackException;
import javax.transaction.SystemException;
import javax.transaction.UserTransaction;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.FormParam;
import javax.ws.rs.Produces;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

/**
 * REST Web Service
 *
 * @author Riccardo De Benedictis
 */
@Path("/")
public class LECTurEResource {

    private static final Logger LOG = Logger.getLogger(LECTurEResource.class.getName());
    private static final Jsonb JSONB = JsonbBuilder.create();
    @PersistenceContext
    private EntityManager em;
    @Resource
    private UserTransaction utx;
    @EJB
    private LECTurEBean ctx;

    /**
     * Creates a new instance of UsersResource
     */
    public LECTurEResource() {
    }

    @POST
    @Path("new_user")
    @Produces(MediaType.APPLICATION_JSON)
    @Consumes(MediaType.APPLICATION_JSON)
    public User newUser(NewUserRequest new_user) {
        try {
            utx.begin();
            UserEntity u = new UserEntity();
            u.setEmail(new_user.email);
            u.setPassword(new_user.password);
            u.setFirstName(new_user.first_name);
            u.setLastName(new_user.last_name);
            em.persist(u);
            ctx.newUser(u.getId());
            utx.commit();
            return new User(u.getId(), u.getEmail(), u.getFirstName(), u.getLastName(), ctx.isOnline(u.getId()), ctx.getParTypes(u.getId()), ctx.getParValues(u.getId()));
        } catch (IllegalStateException | SecurityException | HeuristicMixedException | HeuristicRollbackException | NotSupportedException | RollbackException | SystemException ex) {
            try {
                utx.rollback();
            } catch (IllegalStateException | SecurityException | SystemException ex1) {
                LOG.log(Level.SEVERE, null, ex1);
            }
            throw new WebApplicationException(ex.getMessage());
        }
    }

    @GET
    @Path("users")
    @Produces(MediaType.APPLICATION_JSON)
    public Collection<User> getUsers() {
        List<UserEntity> users = em.createQuery("SELECT u FROM UserEntity u", UserEntity.class).getResultList();
        return users.stream().map(u -> new User(u.getId(), u.getEmail(), u.getFirstName(), u.getLastName(), ctx.isOnline(u.getId()), ctx.getParTypes(u.getId()), ctx.getParValues(u.getId()))).collect(Collectors.toList());
    }

    @GET
    @Path("find_users/{search_string}")
    @Produces(MediaType.APPLICATION_JSON)
    public Collection<User> findUsers(@PathParam("search_string") String search_string) {
        TypedQuery<UserEntity> query = em.createQuery("SELECT u FROM UserEntity u WHERE u.first_name LIKE :search_string OR u.last_name LIKE :search_string", UserEntity.class);
        query.setParameter("search_string", search_string);
        return query.getResultList().stream().map(u -> new User(u.getId(), u.getEmail(), u.getFirstName(), u.getLastName(), ctx.isOnline(u.getId()), null, null)).collect(Collectors.toList());
    }

    @GET
    @Path("users/{user_id}")
    @Produces(MediaType.APPLICATION_JSON)
    public User getUser(@PathParam("user_id") long user_id) {
        UserEntity u = em.find(UserEntity.class, user_id);
        if (u != null) {
            return new User(u.getId(), u.getEmail(), u.getFirstName(), u.getLastName(), ctx.isOnline(u.getId()), ctx.getParTypes(u.getId()), ctx.getParValues(u.getId()));
        } else {
            throw new WebApplicationException("Cannot find user id");
        }
    }

    @DELETE
    @Path("users/{user_id}")
    public void deleteUser(@PathParam("user_id") long user_id) {
        try {
            utx.begin();
            UserEntity u = em.find(UserEntity.class, user_id);
            for (UserEntity student : u.getStudents()) {
                student.removeTeacher(u);
                em.merge(student);
            }
            for (UserEntity teacher : u.getTeachers()) {
                teacher.removeStudent(u);
                em.merge(teacher);
            }
            em.remove(u);
            ctx.deleteUser(user_id);
            utx.commit();
        } catch (NotSupportedException | SystemException | RollbackException | HeuristicMixedException | HeuristicRollbackException | SecurityException | IllegalStateException ex) {
            try {
                utx.rollback();
            } catch (IllegalStateException | SecurityException | SystemException ex1) {
                LOG.log(Level.SEVERE, null, ex1);
            }
            throw new WebApplicationException(ex.getMessage());
        }
    }

    @PUT
    @Path("add_teacher")
    public void addTeacher(@FormParam("student_id") long student_id, @FormParam("teacher_id") long teacher_id) {
        try {
            utx.begin();
            UserEntity student = em.find(UserEntity.class, student_id);
            UserEntity teacher = em.find(UserEntity.class, teacher_id);
            student.addTeacher(teacher);
            teacher.addStudent(student);
            em.merge(student);
            em.merge(teacher);
            ctx.addTeacher(student_id, teacher_id);
            utx.commit();
        } catch (NotSupportedException | SystemException | RollbackException | HeuristicMixedException | HeuristicRollbackException | SecurityException | IllegalStateException ex) {
            try {
                utx.rollback();
            } catch (IllegalStateException | SecurityException | SystemException ex1) {
                LOG.log(Level.SEVERE, null, ex1);
            }
            throw new WebApplicationException(ex.getMessage());
        }
    }

    @PUT
    @Path("remove_teacher")
    public void removeTeacher(@FormParam("student_id") long student_id, @FormParam("teacher_id") long teacher_id) {
        try {
            utx.begin();
            UserEntity student = em.find(UserEntity.class, student_id);
            UserEntity teacher = em.find(UserEntity.class, teacher_id);
            student.removeTeacher(teacher);
            teacher.removeStudent(student);
            em.merge(student);
            em.merge(teacher);
            ctx.removeTeacher(student_id, teacher_id);
            utx.commit();
        } catch (NotSupportedException | SystemException | RollbackException | HeuristicMixedException | HeuristicRollbackException | SecurityException | IllegalStateException ex) {
            try {
                utx.rollback();
            } catch (IllegalStateException | SecurityException | SystemException ex1) {
                LOG.log(Level.SEVERE, null, ex1);
            }
            throw new WebApplicationException(ex.getMessage());
        }
    }

    @POST
    @Path("login")
    @Produces(MediaType.APPLICATION_JSON)
    @Consumes(MediaType.APPLICATION_JSON)
    public InitResponse login(Credentials credentials) {
        try {
            TypedQuery<UserEntity> query = em.createQuery("SELECT u FROM UserEntity u WHERE u.email = :email AND u.password = :password", UserEntity.class);
            query.setParameter("email", credentials.email);
            query.setParameter("password", credentials.password);
            UserEntity u = query.getSingleResult();
            User user = new User(u.getId(), u.getEmail(), u.getFirstName(), u.getLastName(), ctx.isOnline(u.getId()), null, null);
            Collection<LessonModel> models = u.getModels().stream().map(model -> {
                LessonModel m = JSONB.fromJson(model.getModel(), LessonModel.class);
                m.id = model.getId();
                return m;
            }).collect(Collectors.toList());
            // the lessons followed as a teacher..
            List<Lesson> following_lessons = u.getLessons().stream().map(lesson -> {
                Lesson l = ctx.getLessonManager(lesson.getId()).getLesson();
                return new Lesson(l.id, l.teacher_id, l.name, l.state, l.time, l.model, l.roles, null, l.tokens);
            }).collect(Collectors.toList());
            List<User> students = u.getStudents().stream().map(std -> new User(std.getId(), std.getEmail(), std.getFirstName(), std.getLastName(), ctx.isOnline(std.getId()), ctx.getParTypes(std.getId()), ctx.getParValues(std.getId()))).collect(Collectors.toList());
            // the lessons followed as a student..
            List<Lesson> followed_lessons = u.getRoles().stream().map(role -> {
                Lesson l = ctx.getLessonManager(role.getLesson().getId()).getLesson();
                List<Event> events = l.events.stream().filter(e -> e.role.equals(role.getName())).collect(Collectors.toList());
                return new Lesson(l.id, l.teacher_id, l.name, l.state, l.time, null, l.roles, events, null);
            }).collect(Collectors.toList());
            List<User> teachers = u.getTeachers().stream().map(tc -> new User(tc.getId(), tc.getEmail(), tc.getFirstName(), tc.getLastName(), ctx.isOnline(tc.getId()), null, null)).collect(Collectors.toList());

            InitResponse init = new InitResponse(user, followed_lessons, teachers, models, following_lessons, students);
            return init;
        } catch (NoResultException e) {
            throw new WebApplicationException(e.getLocalizedMessage(), Response.Status.UNAUTHORIZED);
        }
    }

    @PUT
    @Path("answer_question")
    public void answerQuestion(@FormParam("lesson_id") long lesson_id, @FormParam("question_id") int question_id, @FormParam("answer_id") int answer_id) {
        try {
            utx.begin();
            ctx.answerQuestion(lesson_id, question_id, answer_id);
            utx.commit();
        } catch (NotSupportedException | SystemException | RollbackException | HeuristicMixedException | HeuristicRollbackException | SecurityException | IllegalStateException ex) {
            try {
                utx.rollback();
            } catch (IllegalStateException | SecurityException | SystemException ex1) {
                LOG.log(Level.SEVERE, null, ex1);
            }
            throw new WebApplicationException(ex.getMessage());
        }
    }

    @POST
    @Path("new_lesson_by_model")
    @Produces(MediaType.APPLICATION_JSON)
    @Consumes(MediaType.APPLICATION_JSON)
    public Lesson newLessonByModel(NewLessonRequest new_lesson) {
        try {
            utx.begin();
            UserEntity teacher = em.find(UserEntity.class, new_lesson.teacher_id);
            LessonModelEntity lme = new LessonModelEntity();
            lme.setModel(JSONB.toJson(new_lesson.model));
            em.persist(lme);
            teacher.addModel(lme);
            LessonEntity le = new LessonEntity();
            le.setName(new_lesson.lesson_name);
            le.setTeacher(teacher);
            le.setModel(lme);
            teacher.addLesson(le);
            for (Map.Entry<String, Long> role : new_lesson.roles.entrySet()) {
                UserEntity student = em.find(UserEntity.class, role.getValue());
                RoleEntity re = new RoleEntity();
                re.setStudent(student);
                re.setLesson(le);
                re.setName(role.getKey());
                le.addRole(re);
                student.addRole(re);
                em.persist(re);
                em.persist(student);
            }
            em.persist(le);
            teacher.addLesson(le);
            em.merge(teacher);

            Lesson l = new Lesson(le.getId(), new_lesson.teacher_id, new_lesson.lesson_name, Lesson.LessonState.Stopped, 0, lme.getId(), new_lesson.roles, Collections.emptyList(), Collections.emptyList());
            ctx.newLesson(l, new_lesson.model);

            utx.commit();
            return l;
        } catch (NotSupportedException | SystemException | RollbackException | HeuristicMixedException | HeuristicRollbackException | SecurityException | IllegalStateException ex) {
            try {
                utx.rollback();
            } catch (IllegalStateException | SecurityException | SystemException ex1) {
                LOG.log(Level.SEVERE, null, ex1);
            }
            throw new WebApplicationException(ex.getMessage());
        }
    }

    @POST
    @Path("new_lesson_by_model_id")
    @Produces(MediaType.APPLICATION_JSON)
    @Consumes(MediaType.APPLICATION_JSON)
    public Lesson newLessonByModelId(NewLessonRequest new_lesson) {
        try {
            utx.begin();
            UserEntity teacher = em.find(UserEntity.class, new_lesson.teacher_id);
            LessonModelEntity lme = em.find(LessonModelEntity.class, new_lesson.lesson_model_id);
            LessonEntity le = new LessonEntity();
            le.setName(new_lesson.lesson_name);
            le.setTeacher(teacher);
            le.setModel(lme);
            teacher.addLesson(le);
            for (Map.Entry<String, Long> role : new_lesson.roles.entrySet()) {
                UserEntity student = em.find(UserEntity.class, role.getValue());
                RoleEntity re = new RoleEntity();
                re.setStudent(student);
                re.setLesson(le);
                re.setName(role.getKey());
                le.addRole(re);
                student.addRole(re);
                em.persist(re);
                em.persist(student);
            }
            em.persist(le);
            teacher.addLesson(le);
            em.merge(teacher);

            Lesson l = new Lesson(le.getId(), new_lesson.teacher_id, new_lesson.lesson_name, Lesson.LessonState.Stopped, 0, lme.getId(), new_lesson.roles, Collections.emptyList(), Collections.emptyList());
            ctx.newLesson(l, JSONB.fromJson(lme.getModel(), LessonModel.class));

            utx.commit();
            return l;
        } catch (NotSupportedException | SystemException | RollbackException | HeuristicMixedException | HeuristicRollbackException | SecurityException | IllegalStateException ex) {
            try {
                utx.rollback();
            } catch (IllegalStateException | SecurityException | SystemException ex1) {
                LOG.log(Level.SEVERE, null, ex1);
            }
            throw new WebApplicationException(ex.getMessage());
        }
    }

    @GET
    @Path("lessons")
    @Produces(MediaType.APPLICATION_JSON)
    public Collection<Lesson> getLessons() {
        LOG.info("Retrieving lessons");
        List<Lesson> lessons = ctx.getLessonManagers().stream().map(lm -> lm.getLesson()).collect(Collectors.toList());
        LOG.log(Level.INFO, "Found {0} lessons", lessons.size());
        return lessons;
    }

    @PUT
    @Path("solve_lesson")
    public void solveLesson(@FormParam("lesson_id") long lesson_id) {
        try {
            utx.begin();
            ctx.solveLesson(lesson_id);
            utx.commit();
        } catch (NotSupportedException | SystemException | RollbackException | HeuristicMixedException | HeuristicRollbackException | SecurityException | IllegalStateException ex) {
            try {
                utx.rollback();
            } catch (IllegalStateException | SecurityException | SystemException ex1) {
                LOG.log(Level.SEVERE, null, ex1);
            }
            throw new WebApplicationException(ex.getMessage());
        }
    }

    @PUT
    @Path("play")
    public void play(@FormParam("lesson_id") long lesson_id) {
        LOG.log(Level.INFO, "Starting lesson {0}", lesson_id);
        try {
            utx.begin();
            ctx.play(lesson_id);
            utx.commit();
        } catch (NotSupportedException | SystemException | RollbackException | HeuristicMixedException | HeuristicRollbackException | SecurityException | IllegalStateException ex) {
            try {
                utx.rollback();
            } catch (IllegalStateException | SecurityException | SystemException ex1) {
                LOG.log(Level.SEVERE, null, ex1);
            }
            throw new WebApplicationException(ex.getMessage());
        }
    }

    @PUT
    @Path("pause")
    public void pause(@FormParam("lesson_id") long lesson_id) {
        LOG.log(Level.INFO, "Pausing lesson {0}", lesson_id);
        try {
            utx.begin();
            ctx.pause(lesson_id);
            utx.commit();
        } catch (NotSupportedException | SystemException | RollbackException | HeuristicMixedException | HeuristicRollbackException | SecurityException | IllegalStateException ex) {
            try {
                utx.rollback();
            } catch (IllegalStateException | SecurityException | SystemException ex1) {
                LOG.log(Level.SEVERE, null, ex1);
            }
            throw new WebApplicationException(ex.getMessage());
        }
    }

    @PUT
    @Path("stop")
    public void stop(@FormParam("lesson_id") long lesson_id) {
        LOG.log(Level.INFO, "Stopping lesson {0}", lesson_id);
        try {
            utx.begin();
            ctx.stop(lesson_id);
            utx.commit();
        } catch (NotSupportedException | SystemException | RollbackException | HeuristicMixedException | HeuristicRollbackException | SecurityException | IllegalStateException ex) {
            try {
                utx.rollback();
            } catch (IllegalStateException | SecurityException | SystemException ex1) {
                LOG.log(Level.SEVERE, null, ex1);
            }
            throw new WebApplicationException(ex.getMessage());
        }
    }

    @PUT
    @Path("go_to")
    public void goTo(@FormParam("lesson_id") long lesson_id, @FormParam("time") long time) {
        try {
            utx.begin();
            ctx.goTo(lesson_id, time);
            utx.commit();
        } catch (NotSupportedException | SystemException | RollbackException | HeuristicMixedException | HeuristicRollbackException | SecurityException | IllegalStateException ex) {
            try {
                utx.rollback();
            } catch (IllegalStateException | SecurityException | SystemException ex1) {
                LOG.log(Level.SEVERE, null, ex1);
            }
            throw new WebApplicationException(ex.getMessage());
        }
    }

    @PUT
    @Path("set_time")
    public void setTime(@FormParam("lesson_id") long lesson_id, @FormParam("token_id") int token_id, @FormParam("time") long time) {
        try {
            utx.begin();
            ctx.setTime(lesson_id, token_id, time);
            utx.commit();
        } catch (NotSupportedException | SystemException | RollbackException | HeuristicMixedException | HeuristicRollbackException | SecurityException | IllegalStateException ex) {
            try {
                utx.rollback();
            } catch (IllegalStateException | SecurityException | SystemException ex1) {
                LOG.log(Level.SEVERE, null, ex1);
            }
            throw new WebApplicationException(ex.getMessage());
        }
    }

    @DELETE
    @Path("lessons/{lesson_id}")
    public void deleteLesson(@PathParam("lesson_id") long lesson_id) {
        try {
            utx.begin();
            LessonEntity lesson = em.find(LessonEntity.class, lesson_id);
            ctx.removeLesson(lesson_id);
            lesson.getTeacher().removeLesson(lesson);
            em.merge(lesson.getTeacher());
            for (RoleEntity role : lesson.getRoles()) {
                role.getStudent().removeRole(role);
                em.merge(role.getStudent());
                em.remove(role);
            }
            em.remove(lesson);
            utx.commit();
        } catch (NotSupportedException | SystemException | RollbackException | HeuristicMixedException | HeuristicRollbackException | SecurityException | IllegalStateException ex) {
            try {
                utx.rollback();
            } catch (IllegalStateException | SecurityException | SystemException ex1) {
                LOG.log(Level.SEVERE, null, ex1);
            }
            throw new WebApplicationException(ex.getMessage());
        }
    }
}
