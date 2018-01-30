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
import it.cnr.istc.ale.api.messages.LostParameter;
import it.cnr.istc.ale.api.messages.LostStudent;
import it.cnr.istc.ale.api.messages.Message;
import it.cnr.istc.ale.api.messages.NewLesson;
import it.cnr.istc.ale.api.messages.NewParameter;
import it.cnr.istc.ale.api.messages.NewStudent;
import it.cnr.istc.ale.api.model.LessonModel;
import it.cnr.istc.ale.server.db.LessonEntity;
import it.cnr.istc.ale.server.db.LessonModelEntity;
import it.cnr.istc.ale.server.db.RoleEntity;
import it.cnr.istc.ale.server.db.UserEntity;
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

/**
 *
 * @author Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
 */
public class Context implements UserAPI, LessonAPI {

    private static final Logger LOG = Logger.getLogger(Context.class.getName());
    private static final EntityManagerFactory emf = Persistence.createEntityManagerFactory("ALE_PU");
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
    }

    public void addConnection(long id) {
        if (!parameter_types.containsKey(id)) {
            parameter_types.put(id, new HashMap<>());
        }
        if (!parameter_values.containsKey(id)) {
            parameter_values.put(id, new HashMap<>());
        }
        try {
            mqtt.publish(id + "/output/on-line", Boolean.TRUE.toString().getBytes(), 1, true);
            mqtt.subscribe(id + "/output", (String topic, MqttMessage message) -> {
                Message m = MAPPER.readValue(message.getPayload(), Message.class);
                if (m instanceof NewParameter) {
                    NewParameter np = (NewParameter) m;
                    Parameter par = np.getParameter();
                    parameter_types.get(id).put(par.getName(), par);
                    mqtt.subscribe(id + "/output/" + par.getName(), (String par_topic, MqttMessage par_value) -> {
                        parameter_values.get(id).put(par.getName(), MAPPER.readValue(par_value.getPayload(), new TypeReference<Map<String, String>>() {
                        }));
                    });
                } else if (m instanceof LostParameter) {
                    LostParameter lp = (LostParameter) m;
                    mqtt.unsubscribe(id + "/output/" + lp.getName());
                    parameter_types.get(id).remove(lp.getName());
                    parameter_values.get(id).remove(lp.getName());
                }
            });
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    public void removeConnection(long id) {
        try {
            for (Map.Entry<String, Parameter> par_type : parameter_types.get(id).entrySet()) {
                mqtt.unsubscribe(id + "/output/" + par_type.getKey());
            }
            mqtt.unsubscribe(id + "/output");
            mqtt.publish(id + "/output/on-line", Boolean.FALSE.toString().getBytes(), 1, true);
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
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void pause_lesson(long lesson_id) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void stop_lesson(long lesson_id) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void go_at(long lesson_id, long timestamp) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
}
