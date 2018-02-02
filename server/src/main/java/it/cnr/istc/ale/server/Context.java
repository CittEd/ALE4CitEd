/*
 * Copyright (C) 2018 Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
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
import com.fasterxml.jackson.databind.ObjectMapper;
import it.cnr.istc.ale.api.Lesson;
import it.cnr.istc.ale.api.LessonAPI;
import it.cnr.istc.ale.api.Parameter;
import it.cnr.istc.ale.api.User;
import it.cnr.istc.ale.api.UserAPI;
import it.cnr.istc.ale.api.messages.EventUpdate;
import it.cnr.istc.ale.api.messages.HideEvent;
import it.cnr.istc.ale.api.messages.LostParameter;
import it.cnr.istc.ale.api.messages.LostStudent;
import it.cnr.istc.ale.api.messages.Message;
import it.cnr.istc.ale.api.messages.NewEvent;
import it.cnr.istc.ale.api.messages.NewLesson;
import it.cnr.istc.ale.api.messages.NewParameter;
import it.cnr.istc.ale.api.messages.NewStudent;
import it.cnr.istc.ale.api.model.LessonModel;
import it.cnr.istc.ale.api.model.QuestionEvent;
import it.cnr.istc.ale.api.model.TextEvent;
import it.cnr.istc.ale.server.db.LessonEntity;
import it.cnr.istc.ale.server.db.LessonModelEntity;
import it.cnr.istc.ale.server.db.RoleEntity;
import it.cnr.istc.ale.server.db.UserEntity;
import it.cnr.istc.ale.server.solver.LessonManager;
import it.cnr.istc.ale.server.solver.Token;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;
import javax.persistence.TypedQuery;
import org.eclipse.paho.client.mqttv3.IMqttDeliveryToken;
import org.eclipse.paho.client.mqttv3.MqttCallback;
import org.eclipse.paho.client.mqttv3.MqttClient;
import org.eclipse.paho.client.mqttv3.MqttConnectOptions;
import org.eclipse.paho.client.mqttv3.MqttException;
import org.eclipse.paho.client.mqttv3.MqttMessage;
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence;
import it.cnr.istc.ale.server.solver.LessonManagerListener;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

/**
 *
 * @author Riccardo De Benedictis
 */
public class Context implements UserAPI, LessonAPI {

    private static final Logger LOG = Logger.getLogger(Context.class.getName());
    private static final EntityManagerFactory emf = Persistence.createEntityManagerFactory("ALE_PU");
    private static final ScheduledExecutorService EXECUTOR = Executors.newScheduledThreadPool(Runtime.getRuntime().availableProcessors());
    public static final String SERVER_ID = "server";
    private static Context context;
    public static final ObjectMapper MAPPER = new ObjectMapper();

    public static Context getContext() {
        if (context == null) {
            context = new Context();
        }
        return context;
    }
    private MqttClient mqtt;
    /**
     * For each user id, a map of parameter types containing the name of the
     * parameter as key.
     */
    private final Map<Long, Map<String, Parameter>> parameter_types = new HashMap<>();
    /**
     * For each user id, a map of parameter values containing the name of the
     * parameter as key. Notice that parameter values are represented through a
     * map.
     */
    private final Map<Long, Map<String, Map<String, String>>> parameter_values = new HashMap<>();
    /**
     * For each lesson, the solver that manages the lesson.
     */
    private final Map<Long, LessonManager> lessons = new HashMap<>();
    private final Map<Long, Boolean> running_lessons = new HashMap<>();
    private final Lock lessons_lock = new ReentrantLock();

    private Context() {
        try {
            mqtt = new MqttClient("tcp://" + Config.getInstance().getParam(Config.Param.Host) + ":" + Config.getInstance().getParam(Config.Param.MQTTPort), SERVER_ID, new MemoryPersistence());
            mqtt.setCallback(new MqttCallback() {
                @Override
                public void connectionLost(Throwable cause) {
                    LOG.log(Level.SEVERE, null, cause);
                }

                @Override
                public void messageArrived(String topic, MqttMessage message) throws Exception {
                }

                @Override
                public void deliveryComplete(IMqttDeliveryToken token) {
                }
            });
            MqttConnectOptions options = new MqttConnectOptions();
            options.setCleanSession(false);
            options.setAutomaticReconnect(true);
            mqtt.connect(options);
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }

        EXECUTOR.scheduleAtFixedRate(() -> {
            lessons_lock.lock();
            try {
                for (Map.Entry<Long, LessonManager> entry : lessons.entrySet()) {
                    if (running_lessons.get(entry.getKey())) {
                        entry.getValue().tick();
                    }
                }
            } finally {
                lessons_lock.unlock();
            }
        }, 0, 1, TimeUnit.SECONDS);
    }

    public void addConnection(long user_id) {
        if (!parameter_types.containsKey(user_id)) {
            parameter_types.put(user_id, new HashMap<>());
        }
        if (!parameter_values.containsKey(user_id)) {
            parameter_values.put(user_id, new HashMap<>());
        }
        try {
            mqtt.publish(user_id + "/output/on-line", Boolean.TRUE.toString().getBytes(), 1, true);
            mqtt.subscribe(user_id + "/output", (String topic, MqttMessage message) -> {
                Message m = MAPPER.readValue(message.getPayload(), Message.class);
                if (m instanceof NewParameter) {
                    // the user has declared a new parameter..
                    NewParameter np = (NewParameter) m;
                    Parameter par = np.getParameter();
                    parameter_types.get(user_id).put(par.getName(), par);
                    mqtt.subscribe(user_id + "/output/" + par.getName(), (String par_topic, MqttMessage par_value) -> {
                        // the user has updated one of his/her parameters..
                        parameter_values.get(user_id).put(par.getName(), MAPPER.readValue(par_value.getPayload(), new TypeReference<Map<String, String>>() {
                        }));
                    });
                } else if (m instanceof LostParameter) {
                    // the user has removed a parameter..
                    LostParameter lp = (LostParameter) m;
                    mqtt.unsubscribe(user_id + "/output/" + lp.getName());
                    parameter_types.get(user_id).remove(lp.getName());
                    parameter_values.get(user_id).remove(lp.getName());
                } else {
                    throw new UnsupportedOperationException("Not supported yet: " + m);
                }
            });
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    public void removeConnection(long user_id) {
        try {
            for (Map.Entry<String, Parameter> par_type : parameter_types.get(user_id).entrySet()) {
                mqtt.unsubscribe(user_id + "/output/" + par_type.getKey());
            }
            mqtt.unsubscribe(user_id + "/output");
            mqtt.publish(user_id + "/output/on-line", Boolean.FALSE.toString().getBytes(), 1, true);
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    @Override
    public User new_user(String email, String password, String first_name, String last_name) {
        EntityManager em = emf.createEntityManager();
        UserEntity ue = new UserEntity();
        ue.setEmail(email);
        ue.setPassword(password);
        ue.setFirstName(first_name);
        ue.setLastName(last_name);
        em.getTransaction().begin();
        em.persist(ue);
        em.getTransaction().commit();
        return new User(ue.getId(), ue.getFirstName(), ue.getLastName());
    }

    @Override
    public User get_user(long user_id) {
        UserEntity ue = emf.createEntityManager().find(UserEntity.class, user_id);
        return new User(ue.getId(), ue.getFirstName(), ue.getLastName());
    }

    @Override
    public Map<String, Parameter> get_parameter_types(long student_id) {
        Map<String, Parameter> types = parameter_types.get(student_id);
        if (types == null) {
            LOG.log(Level.WARNING, "No parameter types for (offline) user {0}", student_id);
            return Collections.emptyMap();
        } else {
            return types;
        }
    }

    @Override
    public Collection<User> find_users(String search_string) {
        EntityManager em = emf.createEntityManager();
        TypedQuery<UserEntity> query = em.createQuery("SELECT u FROM UserEntity u WHERE u.first_name LIKE :search_string OR u.last_name LIKE :search_string", UserEntity.class);
        query.setParameter("search_string", search_string);
        return query.getResultList().stream().map(usr -> new User(usr.getId(), usr.getFirstName(), usr.getLastName())).collect(Collectors.toList());
    }

    @Override
    public User login(String email, String password) {
        EntityManager em = emf.createEntityManager();
        TypedQuery<UserEntity> query = em.createQuery("SELECT u FROM UserEntity u WHERE u.email = :email AND u.password = :password", UserEntity.class);
        query.setParameter("email", email);
        query.setParameter("password", password);
        UserEntity ue = query.getSingleResult();
        return new User(ue.getId(), ue.getFirstName(), ue.getLastName());
    }

    @Override
    public void add_teacher(long student_id, long teacher_id) {
        EntityManager em = emf.createEntityManager();
        em.getTransaction().begin();
        UserEntity student = em.find(UserEntity.class, student_id);
        UserEntity teacher = em.find(UserEntity.class, teacher_id);
        student.addTeacher(teacher);
        teacher.addStudent(student);
        em.persist(student);
        em.getTransaction().commit();
        try {
            mqtt.publish(teacher_id + "/input", MAPPER.writeValueAsBytes(new NewStudent(student_id)), 1, false);
        } catch (JsonProcessingException | MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    @Override
    public void remove_teacher(long student_id, long teacher_id) {
        EntityManager em = emf.createEntityManager();
        em.getTransaction().begin();
        UserEntity student = em.find(UserEntity.class, student_id);
        UserEntity teacher = em.find(UserEntity.class, teacher_id);
        student.removeTeacher(teacher);
        teacher.removeStudent(student);
        em.persist(student);
        em.getTransaction().commit();
        try {
            mqtt.publish(teacher_id + "/input", MAPPER.writeValueAsBytes(new LostStudent(student_id)), 1, false);
        } catch (JsonProcessingException | MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    @Override
    public Collection<User> get_teachers(long student_id) {
        return emf.createEntityManager().find(UserEntity.class, student_id).getTeachers().stream().map(st -> new User(st.getId(), st.getFirstName(), st.getLastName())).collect(Collectors.toList());
    }

    @Override
    public Collection<User> get_students(long teacher_id) {
        return emf.createEntityManager().find(UserEntity.class, teacher_id).getStudents().stream().map(st -> new User(st.getId(), st.getFirstName(), st.getLastName())).collect(Collectors.toList());
    }

    @Override
    public Lesson new_lesson(long teacher_id, String lesson_name, String model, String roles) {
        try {
            LessonModel lesson_model = MAPPER.readValue(model, LessonModel.class);
            Map<String, Long> lesson_roles = MAPPER.readValue(roles, new TypeReference<Map<String, Long>>() {
            });

            EntityManager em = emf.createEntityManager();
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
                public void newToken(Token tk) {
                    try {
                        // we notify the teacher that a new event has been created..
                        mqtt.publish(teacher_id + "/input", MAPPER.writeValueAsBytes(new NewEvent(l.getId(), tk.tp, tk.cause != null ? (long) tk.cause.tp : null, (long) lm.network.getValue(tk.tp), tk.event.getName())), 1, false);
                    } catch (JsonProcessingException | MqttException ex) {
                        LOG.log(Level.SEVERE, null, ex);
                    }
                }

                @Override
                public void movedToken(Token tk) {
                    try {
                        // we notify the teacher that an event has been updated..
                        mqtt.publish(teacher_id + "/input", MAPPER.writeValueAsBytes(new EventUpdate(l.getId(), tk.tp, (long) lm.network.getValue(tk.tp))), 1, false);
                    } catch (JsonProcessingException | MqttException ex) {
                        LOG.log(Level.SEVERE, null, ex);
                    }
                }

                @Override
                public void executeToken(Token tk) {
                    try {
                        byte[] execute_event_bytes = null;
                        if (tk.event instanceof TextEvent) {
                            execute_event_bytes = MAPPER.writeValueAsBytes(new it.cnr.istc.ale.api.messages.TextEvent(l.getId(), tk.tp, ((TextEvent) tk.event).getContent()));
                        } else if (tk.event instanceof QuestionEvent) {
                            Collection<String> answers = new ArrayList<>(((QuestionEvent) tk.event).getAnswers().size());
                            for (QuestionEvent.Answer answer : ((QuestionEvent) tk.event).getAnswers()) {
                                answers.add(MAPPER.writeValueAsString(answer));
                            }
                            execute_event_bytes = MAPPER.writeValueAsBytes(new it.cnr.istc.ale.api.messages.QuestionEvent(l.getId(), tk.tp, ((QuestionEvent) tk.event).getQuestion(), answers));
                        } else {
                            throw new UnsupportedOperationException("Not supported yet.");
                        }
                        try {
                            // we notify the teacher that a token has to be executed..
                            mqtt.publish(teacher_id + "/input", execute_event_bytes, 1, false);
                        } catch (MqttException ex) {
                            LOG.log(Level.SEVERE, null, ex);
                        }
                        try {
                            // we notify the student associated to the token's role that a token has to be executed..
                            mqtt.publish(lesson_roles.get(tk.event.getRole()) + "/input", execute_event_bytes, 1, false);
                        } catch (MqttException ex) {
                            LOG.log(Level.SEVERE, null, ex);
                        }
                    } catch (JsonProcessingException ex) {
                        LOG.log(Level.SEVERE, null, ex);
                    }
                }

                @Override
                public void hideToken(Token tk) {
                    try {
                        byte[] hide_event_bytes = MAPPER.writeValueAsBytes(new HideEvent(l.getId(), tk.tp));
                        try {
                            // we notify the teacher that a token has to be hidden..
                            mqtt.publish(teacher_id + "/input", hide_event_bytes, 1, false);
                        } catch (MqttException ex) {
                            LOG.log(Level.SEVERE, null, ex);
                        }
                        try {
                            // we notify the student associated to the token's role that a token has to be hidden..
                            mqtt.publish(lesson_roles.get(tk.event.getRole()) + "/input", hide_event_bytes, 1, false);
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
                        mqtt.publish(teacher_id + "/input/lesson-" + l.getId() + "/time", Long.toString(time).getBytes(), 1, true);
                    } catch (MqttException ex) {
                        LOG.log(Level.SEVERE, null, ex);
                    }
                }
            });
            lm.setModel(lesson_model);
            lessons.put(l.getId(), lm);
            running_lessons.put(l.getId(), Boolean.FALSE);

            // we notify all the students that a new lesson has been created..
            for (Long student_id : lesson_roles.values()) {
                try {
                    mqtt.publish(student_id + "/input", MAPPER.writeValueAsBytes(new NewLesson(l)), 1, false);
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
    public Collection<Lesson> get_lessons(long teacher_id) {
        EntityManager em = emf.createEntityManager();
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
    public Collection<LessonModel> get_models(long teacher_id) {
        EntityManager em = emf.createEntityManager();
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
    public Collection<Lesson> get_followed_lessons(long student_id) {
        EntityManager em = emf.createEntityManager();
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
    public void start_lesson(long lesson_id) {
        lessons_lock.lock();
        try {
            running_lessons.put(lesson_id, Boolean.TRUE);
        } finally {
            lessons_lock.unlock();
        }
    }

    @Override
    public void pause_lesson(long lesson_id) {
        lessons_lock.lock();
        try {
            running_lessons.put(lesson_id, Boolean.FALSE);
        } finally {
            lessons_lock.unlock();
        }
    }

    @Override
    public void stop_lesson(long lesson_id) {
        lessons_lock.lock();
        try {
            running_lessons.put(lesson_id, Boolean.FALSE);
            lessons.get(lesson_id).goTo(0);
        } finally {
            lessons_lock.unlock();
        }
    }

    @Override
    public void go_at(long lesson_id, long timestamp) {
        lessons_lock.lock();
        try {
            lessons.get(lesson_id).goTo(timestamp);
        } finally {
            lessons_lock.unlock();
        }
    }
}
