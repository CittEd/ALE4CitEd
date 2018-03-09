/*
 * Copyright (C) 2018 ISTC - CNR
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
package it.cnr.istc.lecture.desktopapp;

import it.cnr.istc.lecture.api.Credentials;
import it.cnr.istc.lecture.api.InitResponse;
import it.cnr.istc.lecture.api.Lesson;
import it.cnr.istc.lecture.api.NewUserRequest;
import it.cnr.istc.lecture.api.Parameter;
import it.cnr.istc.lecture.api.User;
import it.cnr.istc.lecture.api.messages.Event;
import it.cnr.istc.lecture.api.messages.LostParameter;
import it.cnr.istc.lecture.api.messages.NewParameter;
import it.cnr.istc.lecture.api.model.LessonModel;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import javafx.application.Platform;
import javafx.beans.Observable;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.stage.Stage;
import javax.json.bind.Jsonb;
import javax.json.bind.JsonbBuilder;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.Form;
import javax.ws.rs.core.GenericType;
import javax.ws.rs.core.MediaType;
import org.eclipse.paho.client.mqttv3.IMqttDeliveryToken;
import org.eclipse.paho.client.mqttv3.MqttCallback;
import org.eclipse.paho.client.mqttv3.MqttClient;
import org.eclipse.paho.client.mqttv3.MqttConnectOptions;
import org.eclipse.paho.client.mqttv3.MqttException;
import org.eclipse.paho.client.mqttv3.MqttMessage;
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence;

/**
 *
 * @author Riccardo De Benedictis
 */
public class Context {

    private static final Logger LOG = Logger.getLogger(Context.class.getName());
    public static final Jsonb JSONB = JsonbBuilder.create();
    private static Context ctx;

    public static Context getContext() {
        if (ctx == null) {
            ctx = new Context();
        }
        return ctx;
    }
    private final Properties properties = new Properties();
    private final Client client = ClientBuilder.newClient();
    private final WebTarget target;
    private MqttClient mqtt;
    private Stage stage;
    /**
     * The current user.
     */
    private final ObjectProperty<User> user = new SimpleObjectProperty<>();
    /**
     * The current user's parameter types.
     */
    private final ObservableList<Parameter> par_types = FXCollections.observableArrayList();
    private final Map<String, Parameter> id_par_types = new HashMap<>();
    /**
     * The current user's parameter values.
     */
    private final Map<String, Map<String, StringProperty>> par_vals = new HashMap<>();
    /**
     * The current user's parameter values as a list, to be displayed on tables.
     * Notice that each parameter can aggregate more than a single value.
     */
    private final ObservableList<ParameterValue> par_values = FXCollections.observableArrayList();
    /**
     * The received events.
     */
    private final ObservableList<Event> events = FXCollections.observableArrayList();
    /**
     * The lessons followed as a student.
     */
    private final ObservableList<FollowingLessonContext> following_lessons = FXCollections.observableArrayList();
    private final Map<Long, FollowingLessonContext> id_following_lessons = new HashMap<>();
    /**
     * The followed teachers.
     */
    private final ObservableList<TeacherContext> teachers = FXCollections.observableArrayList(tch_ctx -> new Observable[]{tch_ctx.onlineProperty()});
    private final Map<Long, TeacherContext> id_teachers = new HashMap<>();
    /**
     * The lessons followed as a teacher.
     */
    private final ObservableList<TeachingLessonContext> teaching_lessons = FXCollections.observableArrayList();
    private final Map<Long, TeachingLessonContext> id_teaching_lessons = new HashMap<>();
    /**
     * The following students.
     */
    private final ObservableList<StudentContext> students = FXCollections.observableArrayList(std_ctx -> new Observable[]{std_ctx.onlineProperty()});
    private final Map<Long, StudentContext> id_students = new HashMap<>();

    private Context() {
        try {
            properties.load(Thread.currentThread().getContextClassLoader().getResourceAsStream("config.properties"));
        } catch (IOException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
        this.target = client.target("http://" + properties.getProperty("host", "localhost") + ":" + properties.getProperty("service-port", "8080")).path("LECTurE-WebApp").path("LECTurE");
        user.addListener((ObservableValue<? extends User> observable, User oldValue, User newValue) -> {
            if (oldValue != null) {
                // we clear the current data..
                try {
                    for (Parameter par : par_types) {
                        // we broadcast the lost of a parameter..
                        mqtt.publish(oldValue.id + "/output", JSONB.toJson(new LostParameter(par.name)).getBytes(), 1, false);
                    }
                    mqtt.disconnect();
                    mqtt.close();
                    par_values.clear();
                    par_vals.clear();
                    par_types.clear();
                } catch (MqttException ex) {
                    LOG.log(Level.SEVERE, null, ex);
                }
            }
            if (newValue != null) {
                // we set up a new user..
                try {
                    mqtt = new MqttClient("tcp://" + properties.getProperty("host", "localhost") + ":" + properties.getProperty("mqtt-port", "1883"), String.valueOf(newValue.id), new MemoryPersistence());
                    mqtt.setCallback(new MqttCallback() {
                        @Override
                        public void connectionLost(Throwable cause) {
                            LOG.log(Level.SEVERE, null, cause);
                            user.set(null);
                        }

                        @Override
                        public void messageArrived(String topic, MqttMessage message) throws Exception {
                            LOG.log(Level.WARNING, "message arrived: {0} - {1}", new Object[]{topic, message});
                        }

                        @Override
                        public void deliveryComplete(IMqttDeliveryToken token) {
                        }
                    });

                    MqttConnectOptions options = new MqttConnectOptions();
                    options.setCleanSession(true);
                    options.setAutomaticReconnect(true);
                    mqtt.connect(options);

                    for (Parameter par : newValue.par_types.values()) {
                        par_types.add(par);
                        // we broadcast the existence of a new parameter..
                        mqtt.publish(newValue.id + "/output", JSONB.toJson(new NewParameter(par)).getBytes(), 1, false);
                    }
                    for (Map.Entry<String, Map<String, String>> par_val : newValue.par_values.entrySet()) {
                        Map<String, StringProperty> c_vals = new HashMap<>();
                        for (Map.Entry<String, String> val : par_val.getValue().entrySet()) {
                            SimpleStringProperty val_prop = new SimpleStringProperty(val.getValue());
                            c_vals.put(val.getKey(), val_prop);
                            par_values.add(new ParameterValue(par_val.getKey() + "." + val.getKey(), val_prop));
                        }
                        par_vals.put(par_val.getKey(), c_vals);
                        // we broadcast the the new value of the parameter..
                        mqtt.publish(newValue.id + "/output/" + par_val.getKey(), JSONB.toJson(par_val.getValue()).getBytes(), 1, true);
                    }
                } catch (MqttException ex) {
                    LOG.log(Level.SEVERE, null, ex);
                }
            }
        });

        following_lessons.addListener((ListChangeListener.Change<? extends FollowingLessonContext> c) -> {
            while (c.next()) {
                c.getAddedSubList().forEach(flc -> id_following_lessons.put(flc.getLesson().id, flc));
                c.getRemoved().forEach(flc -> id_following_lessons.remove(flc.getLesson().id));
            }
        });

        teachers.addListener((ListChangeListener.Change<? extends TeacherContext> c) -> {
            while (c.next()) {
                for (TeacherContext tch_ctx : c.getAddedSubList()) {
                    try {
                        mqtt.subscribe(tch_ctx.getTeacher().id + "/output/on-line", (String topic, MqttMessage message) -> Platform.runLater(() -> tch_ctx.onlineProperty().set(Boolean.parseBoolean(new String(message.getPayload())))));
                    } catch (MqttException ex) {
                        LOG.log(Level.SEVERE, null, ex);
                    }
                    id_teachers.put(tch_ctx.getTeacher().id, tch_ctx);
                }
                for (TeacherContext tch_ctx : c.getRemoved()) {
                    try {
                        mqtt.unsubscribe(tch_ctx.getTeacher().id + "/output/on-line");
                    } catch (MqttException ex) {
                        LOG.log(Level.SEVERE, null, ex);
                    }
                    id_teachers.remove(tch_ctx.getTeacher().id);
                }
            }
        });

        teaching_lessons.addListener((ListChangeListener.Change<? extends TeachingLessonContext> c) -> {
            while (c.next()) {
                c.getAddedSubList().forEach(tlc -> id_teaching_lessons.put(tlc.getLesson().id, tlc));
                c.getRemoved().forEach(tlc -> id_teaching_lessons.remove(tlc.getLesson().id));
            }
        });

        students.addListener((ListChangeListener.Change<? extends StudentContext> c) -> {
            while (c.next()) {
                for (StudentContext std_ctx : c.getAddedSubList()) {
                    try {
                        mqtt.subscribe(std_ctx.getStudent().id + "/output/on-line", (String topic, MqttMessage message) -> Platform.runLater(() -> std_ctx.onlineProperty().set(Boolean.parseBoolean(new String(message.getPayload())))));
                    } catch (MqttException ex) {
                        LOG.log(Level.SEVERE, null, ex);
                    }
                    id_students.put(std_ctx.getStudent().id, std_ctx);
                }
                for (StudentContext std_ctx : c.getRemoved()) {
                    try {
                        mqtt.unsubscribe(std_ctx.getStudent().id + "/output/on-line");
                    } catch (MqttException ex) {
                        LOG.log(Level.SEVERE, null, ex);
                    }
                    id_students.remove(std_ctx.getStudent().id);
                }
            }
        });

        par_types.addListener((ListChangeListener.Change<? extends Parameter> c) -> {
            while (c.next()) {
                for (Parameter par : c.getAddedSubList()) {
                    id_par_types.put(par.name, par);
                }
                for (Parameter par : c.getRemoved()) {
                    id_par_types.remove(par.name);
                }
            }
        });
    }

    public Stage getStage() {
        return stage;
    }

    public void setStage(Stage stage) {
        this.stage = stage;
    }

    public User getUser() {
        return user.get();
    }

    public void setUser(User user) {
        this.user.set(user);
    }

    public ObjectProperty<User> userProperty() {
        return user;
    }

    public ObservableList<ParameterValue> parametersProperty() {
        return par_values;
    }

    public void setParameterValue(String par_name, String sub_par, String value) {
        par_vals.get(par_name).get(sub_par).set(value);
        Map<String, String> val = new HashMap<>();
        for (Map.Entry<String, StringProperty> v_val : par_vals.get(par_name).entrySet()) {
            val.put(v_val.getKey(), v_val.getValue().get());
        }
        try {
            mqtt.publish(user.get().id + "/output/" + par_name, JSONB.toJson(val).getBytes(), 1, true);
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    public ObservableList<Event> eventsProperty() {
        return events;
    }

    public ObservableList<FollowingLessonContext> followingLessonsProperty() {
        return following_lessons;
    }

    public void addTeacher(User teacher) {
        Form form = new Form();
        form.param("student_id", Long.toString(user.get().id));
        form.param("teacher_id", Long.toString(teacher.id));
        target.path("add_teacher")
                .request(MediaType.APPLICATION_JSON)
                .put(Entity.form(form));
        teachers.add(new TeacherContext(teacher));
    }

    public void removeTeacher(TeacherContext tch_ctx) {
        Form form = new Form();
        form.param("student_id", Long.toString(user.get().id));
        form.param("teacher_id", Long.toString(tch_ctx.getTeacher().id));
        target.path("remove_teacher")
                .request(MediaType.APPLICATION_JSON)
                .put(Entity.form(form));
        teachers.remove(tch_ctx);
    }

    public ObservableList<TeacherContext> teachersProperty() {
        return teachers;
    }

    public void addLesson(String lesson_name, LessonModel model, Map<String, Long> roles) {
        Form form = new Form();
        form.param("teacher_id", Long.toString(user.get().id));
        form.param("lesson_name", lesson_name);
        form.param("model", JSONB.toJson(model));
        form.param("roles", JSONB.toJson(roles));
        Lesson lesson = target.path("lessons")
                .path("new_lesson_by_model")
                .request(MediaType.APPLICATION_JSON)
                .post(Entity.form(form), Lesson.class);
        teaching_lessons.add(new TeachingLessonContext(lesson, model));
    }

    public void removeLesson(TeachingLessonContext l_ctx) {
        Form form = new Form();
        form.param("teacher_id", Long.toString(user.get().id));
        form.param("lesson_id", Long.toString(l_ctx.getLesson().id));
        target.path("remove_lesson")
                .request(MediaType.APPLICATION_JSON)
                .post(Entity.form(form));
        teaching_lessons.remove(l_ctx);
    }

    public ObservableList<TeachingLessonContext> teachingLessonsProperty() {
        return teaching_lessons;
    }

    public ObservableList<StudentContext> studentsProperty() {
        return students;
    }

    public void login(String email, String password) {
        Map<String, Parameter> par_tps = load_pars();
        Map<String, Map<String, String>> par_vls = load_par_vals();
        Credentials credentials = new Credentials(email, password, par_tps, par_vls);
        InitResponse init = target.path("login").request(MediaType.APPLICATION_JSON).post(Entity.json(credentials), InitResponse.class);
        init.user.par_types = par_tps;
        init.user.par_values = par_vls;
        user.set(init.user);

        // we add the teachers..
        init.teachers.forEach((teacher) -> teachers.add(new TeacherContext(teacher)));

        // we add the students..
        init.students.forEach((student) -> students.add(new StudentContext(student)));
    }

    public void logout() {
        user.set(null);
    }

    public void newUser(String email, String password, String first_name, String last_name) {
        Map<String, Parameter> par_tps = load_pars();
        Map<String, Map<String, String>> par_vls = load_par_vals();
        NewUserRequest new_user = new NewUserRequest(email, password, first_name, last_name, par_tps, par_vls);
        User u = target.path("new_user").request(MediaType.APPLICATION_JSON).post(Entity.json(new_user), User.class);
        u.par_types = par_tps;
        u.par_values = par_vls;
        user.set(u);
    }

    public Collection<User> findUsers(String search_string) {
        return target.path("find_users").path(search_string).request(MediaType.APPLICATION_JSON).get(new GenericType<Collection<User>>() {
        });
    }

    private static Map<String, Parameter> load_pars() {
        Collection<Parameter> pars = JSONB.fromJson(Context.class.getResourceAsStream("/parameters/types.json"), new ArrayList<Parameter>() {
        }.getClass().getGenericSuperclass());
        return pars.stream().collect(Collectors.toMap(p -> p.name, p -> p));
    }

    private static Map<String, Map<String, String>> load_par_vals() {
        Map<String, Map<String, String>> par_vals = JSONB.fromJson(Context.class.getResourceAsStream("/parameters/values.json"), new HashMap<String, Map<String, String>>() {
        }.getClass().getGenericSuperclass());
        return par_vals;
    }

    public static class ParameterValue {

        private final StringProperty name;
        private final StringProperty value;

        ParameterValue(String name, StringProperty value) {
            this.name = new SimpleStringProperty(name);
            this.value = value;
        }

        public StringProperty nameProperty() {
            return name;
        }

        public StringProperty valueProperty() {
            return value;
        }
    }
}
