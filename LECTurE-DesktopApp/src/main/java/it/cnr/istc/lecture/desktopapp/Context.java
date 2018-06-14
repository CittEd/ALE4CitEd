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
import it.cnr.istc.lecture.api.Lesson.LessonState;
import it.cnr.istc.lecture.api.NewLessonRequest;
import it.cnr.istc.lecture.api.NewUserRequest;
import it.cnr.istc.lecture.api.Parameter;
import it.cnr.istc.lecture.api.User;
import it.cnr.istc.lecture.api.messages.Event;
import it.cnr.istc.lecture.api.messages.HideEvent;
import it.cnr.istc.lecture.api.messages.LostLesson;
import it.cnr.istc.lecture.api.messages.LostParameter;
import it.cnr.istc.lecture.api.messages.Message;
import it.cnr.istc.lecture.api.messages.EventAdapter;
import it.cnr.istc.lecture.api.messages.LostStudent;
import it.cnr.istc.lecture.api.messages.NewLesson;
import it.cnr.istc.lecture.api.messages.NewParameter;
import it.cnr.istc.lecture.api.messages.NewStudent;
import it.cnr.istc.lecture.api.messages.QuestionEvent;
import it.cnr.istc.lecture.api.messages.RemoveToken;
import it.cnr.istc.lecture.api.messages.Token;
import it.cnr.istc.lecture.api.messages.TokenUpdate;
import it.cnr.istc.lecture.api.model.LessonModel;
import it.cnr.istc.lecture.desktopapp.TeachingLessonContext.TokenRow;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
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
import javax.json.bind.JsonbConfig;
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
    public static final Jsonb JSONB = JsonbBuilder.create(new JsonbConfig().withAdapters(new EventAdapter()));
    private static ScheduledExecutorService EXECUTOR = Executors.newSingleThreadScheduledExecutor();
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
    private final Map<String, Map<String, ParameterValue>> par_vals = new HashMap<>();
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
    private final ObservableList<FollowingLessonContext> following_lessons = FXCollections.observableArrayList(l_ctx -> new Observable[]{l_ctx.stateProperty()});
    private final Map<Long, FollowingLessonContext> id_following_lessons = new HashMap<>();
    /**
     * The followed teachers.
     */
    private final ObservableList<TeacherContext> teachers = FXCollections.observableArrayList(tch_ctx -> new Observable[]{tch_ctx.onlineProperty()});
    private final Map<Long, TeacherContext> id_teachers = new HashMap<>();
    /**
     * The lesson models associated to the teacher.
     */
    private final ObservableList<LessonModel> models = FXCollections.observableArrayList();
    /**
     * The lessons followed as a teacher.
     */
    private final ObservableList<TeachingLessonContext> teaching_lessons = FXCollections.observableArrayList(l_ctx -> new Observable[]{l_ctx.stateProperty()});
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
        this.target = client.target("http://" + properties.getProperty("lecture-host", "localhost") + ":" + properties.getProperty("service-port", "8080")).path("LECTurE-WebApp-1.0").path("LECTurE");
        user.addListener((ObservableValue<? extends User> observable, User oldValue, User newValue) -> {
            if (oldValue != null) {
                // we clear the current data..
                try {
                    par_values.clear();
                    par_vals.clear();
                    for (Parameter par : par_types) {
                        // we broadcast the lost of a parameter..
                        mqtt.publish(oldValue.id + "/output", JSONB.toJson(new LostParameter(par.name)).getBytes(), 1, false);
                    }
                    par_types.clear();
                    events.clear();
                    for (FollowingLessonContext l_ctx : following_lessons) {
                        // we unsubscribe from the lesson's time and state..
                        mqtt.unsubscribe(oldValue.id + "/input/lesson-" + l_ctx.getLesson().id + "/time");
                        mqtt.unsubscribe(oldValue.id + "/input/lesson-" + l_ctx.getLesson().id + "/state");
                    }
                    following_lessons.clear();
                    teachers.clear();
                    models.clear();
                    for (TeachingLessonContext l_ctx : teaching_lessons) {
                        l_ctx.tokensProperty().clear();
                        // we unsubscribe from the lesson's time and state..
                        mqtt.unsubscribe(oldValue.id + "/input/lesson-" + l_ctx.getLesson().id + "/time");
                        mqtt.unsubscribe(oldValue.id + "/input/lesson-" + l_ctx.getLesson().id + "/state");
                    }
                    teaching_lessons.clear();
                    students.clear();
                    mqtt.disconnect();
                    mqtt.close();
                } catch (MqttException ex) {
                    LOG.log(Level.SEVERE, null, ex);
                }

                EXECUTOR.shutdown();
            }
            if (newValue != null) {
                // we set up a new user..
                try {
                    mqtt = new MqttClient("tcp://" + properties.getProperty("lecture-host", "localhost") + ":" + properties.getProperty("mqtt-port", "1883"), String.valueOf(newValue.id), new MemoryPersistence());
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

                    mqtt.subscribe(newValue.id + "/input", (String topic, MqttMessage message) -> {
                        LOG.log(Level.INFO, "message arrived: {0} - {1}", new Object[]{topic, message});
                        Message m = JSONB.fromJson(new String(message.getPayload()), Message.class);
                        switch (m.message_type) {
                            case NewStudent:
                                // a new student is following this user..
                                NewStudent new_student = JSONB.fromJson(new String(message.getPayload()), NewStudent.class);
                                Platform.runLater(() -> students.add(new StudentContext(new_student.student)));
                                break;
                            case LostStudent:
                                // a student is not following this user anymore..
                                LostStudent lost_student = JSONB.fromJson(new String(message.getPayload()), LostStudent.class);
                                Platform.runLater(() -> students.remove(id_students.get(lost_student.student_id)));
                                break;
                            case NewLesson:
                                // a teacher has created a new lesson for this student..
                                NewLesson new_lesson = JSONB.fromJson(new String(message.getPayload()), NewLesson.class);
                                Platform.runLater(() -> following_lessons.add(new FollowingLessonContext(new_lesson.lesson)));
                                break;
                            case LostLesson:
                                // a teacher has removed a new lesson for this student..
                                LostLesson lost_lesson = JSONB.fromJson(new String(message.getPayload()), LostLesson.class);
                                Platform.runLater(() -> following_lessons.remove(id_following_lessons.get(lost_lesson.lesson_id)));
                                break;
                            case Token:
                                // a new token has been created for a teaching lesson..
                                Token token = JSONB.fromJson(new String(message.getPayload()), Token.class);
                                Platform.runLater(() -> id_teaching_lessons.get(token.lesson_id).tokensProperty().add(new TeachingLessonContext.TokenRow(token.id, id_teaching_lessons.get(token.lesson_id).timeProperty(), token.min, token.max, token.time, token.refEvent)));
                                break;
                            case TokenUpdate:
                                // a token of a teaching lesson has been updated..
                                TokenUpdate token_update = JSONB.fromJson(new String(message.getPayload()), TokenUpdate.class);
                                Platform.runLater(() -> {
                                    id_teaching_lessons.get(token_update.lesson_id).getToken(token_update.id).timeProperty().setValue(token_update.time);
                                    id_teaching_lessons.get(token_update.lesson_id).getToken(token_update.id).minProperty().setValue(token_update.min);
                                    id_teaching_lessons.get(token_update.lesson_id).getToken(token_update.id).maxProperty().setValue(token_update.max);
                                });
                                break;
                            case RemoveToken:
                                // a token of a teaching lesson has been removed..
                                RemoveToken remove_token = JSONB.fromJson(new String(message.getPayload()), RemoveToken.class);
                                Platform.runLater(() -> id_teaching_lessons.get(remove_token.lesson_id).tokensProperty().remove(id_teaching_lessons.get(remove_token.lesson_id).getToken((int) remove_token.event_id)));
                                break;
                            case Event:
                                // a new event has been created for a following lesson..
                                Event event = JSONB.fromJson(new String(message.getPayload()), Event.class);
                                Platform.runLater(() -> id_following_lessons.get(event.lesson_id).eventsProperty().add(event));
                                break;
                            case HideEvent:
                                // an event has been removed for a following lesson..
                                HideEvent hide_event = JSONB.fromJson(new String(message.getPayload()), HideEvent.class);
                                Platform.runLater(() -> id_following_lessons.get(hide_event.lesson_id).eventsProperty().removeIf(e -> e.event_id == hide_event.event_id));
                                break;
                            case Answer:
                                break;
                            default:
                                throw new AssertionError(m.message_type.name());
                        }
                    });

                    for (Parameter par : newValue.par_types.values()) {
                        par_types.add(par);
                        // we broadcast the existence of a new parameter..
                        mqtt.publish(newValue.id + "/output", JSONB.toJson(new NewParameter(par)).getBytes(), 1, false);
                    }
                    for (Map.Entry<String, Map<String, String>> par_val : newValue.par_values.entrySet()) {
                        Map<String, ParameterValue> c_vals = new HashMap<>();
                        for (Map.Entry<String, String> val : par_val.getValue().entrySet()) {
                            ParameterValue val_prop = new ParameterValue(par_val.getKey() + "." + val.getKey(), val.getValue());
                            c_vals.put(val.getKey(), val_prop);
                            par_values.add(val_prop);
                        }
                        par_vals.put(par_val.getKey(), c_vals);
                        // we broadcast the the new value of the parameter..
                        mqtt.publish(newValue.id + "/output/" + par_val.getKey(), JSONB.toJson(par_val.getValue()).getBytes(), 1, true);
                    }
                } catch (MqttException ex) {
                    LOG.log(Level.SEVERE, null, ex);
                }

                // we simulate the passing of time..
                EXECUTOR.scheduleAtFixedRate(() -> students.forEach(std_ctx -> std_ctx.parametersProperty().forEach(par_val -> Platform.runLater(() -> par_val.updatesProperty().add(new ParUpdate(System.currentTimeMillis(), par_val.value.get()))))), 0, 1, TimeUnit.SECONDS);
            }
        });

        following_lessons.addListener((ListChangeListener.Change<? extends FollowingLessonContext> c) -> {
            while (c.next()) {
                for (FollowingLessonContext flc : c.getAddedSubList()) {
                    id_following_lessons.put(flc.getLesson().id, flc);
                    flc.eventsProperty().addAll(flc.getLesson().events);
                    try {
                        // we subscribe to the lesson's time..
                        mqtt.subscribe(flc.getLesson().teacher_id + "/input/lesson-" + flc.getLesson().id + "/time", (String topic, MqttMessage message) -> {
                            Platform.runLater(() -> flc.timeProperty().setValue(Long.parseLong(new String(message.getPayload()))));
                        });
                        // we subscribe to the lesson's state..
                        mqtt.subscribe(flc.getLesson().teacher_id + "/input/lesson-" + flc.getLesson().id + "/state", (String topic, MqttMessage message) -> {
                            Platform.runLater(() -> flc.stateProperty().setValue(LessonState.valueOf(new String(message.getPayload()))));
                        });
                    } catch (MqttException ex) {
                        LOG.log(Level.SEVERE, null, ex);
                    }
                }
                for (FollowingLessonContext flc : c.getRemoved()) {
                    id_following_lessons.remove(flc.getLesson().id);
                    events.removeAll(flc.eventsProperty());
                    flc.eventsProperty().clear();
                    if (user.isNotNull().get()) {
                        try {
                            // we subscribe from the lesson's time and state..
                            mqtt.unsubscribe(user.get().id + "/input/lesson-" + flc.getLesson().id + "/time");
                            mqtt.unsubscribe(user.get().id + "/input/lesson-" + flc.getLesson().id + "/state");
                        } catch (MqttException ex) {
                            LOG.log(Level.SEVERE, null, ex);
                        }
                    }
                }
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
                for (TeachingLessonContext tlc : c.getAddedSubList()) {
                    id_teaching_lessons.put(tlc.getLesson().id, tlc);
                    try {
                        // we subscribe to the lesson's time..
                        mqtt.subscribe(user.get().id + "/input/lesson-" + tlc.getLesson().id + "/time", (String topic, MqttMessage message) -> {
                            Platform.runLater(() -> tlc.timeProperty().setValue(Long.parseLong(new String(message.getPayload()))));
                        });
                        // we subscribe to the lesson's state..
                        mqtt.subscribe(user.get().id + "/input/lesson-" + tlc.getLesson().id + "/state", (String topic, MqttMessage message) -> {
                            Platform.runLater(() -> tlc.stateProperty().setValue(LessonState.valueOf(new String(message.getPayload()))));
                        });
                    } catch (MqttException ex) {
                        LOG.log(Level.SEVERE, null, ex);
                    }
                }
                for (TeachingLessonContext tlc : c.getRemoved()) {
                    id_teaching_lessons.remove(tlc.getLesson().id);
                    if (user.isNotNull().get()) {
                        try {
                            // we unsubscribe from the lesson's time and state..
                            mqtt.unsubscribe(user.get().id + "/input/lesson-" + tlc.getLesson().id + "/time");
                            mqtt.unsubscribe(user.get().id + "/input/lesson-" + tlc.getLesson().id + "/state");
                        } catch (MqttException ex) {
                            LOG.log(Level.SEVERE, null, ex);
                        }
                    }
                }
            }
        });

        students.addListener((ListChangeListener.Change<? extends StudentContext> c) -> {
            while (c.next()) {
                for (StudentContext std_ctx : c.getAddedSubList()) {
                    long student_id = std_ctx.getStudent().id;
                    try {
                        // we subscribe to be notified whether the student gets online/offline..
                        mqtt.subscribe(student_id + "/output/on-line", (String topic, MqttMessage message) -> Platform.runLater(() -> std_ctx.onlineProperty().set(Boolean.parseBoolean(new String(message.getPayload())))));
                        // we subscribe/unsubscribe to the student's added/removed parameters..
                        std_ctx.parameterTypesProperty().addListener((ListChangeListener.Change<? extends Parameter> c1) -> {
                            while (c1.next()) {
                                // we subscribe to the new user's parameters..
                                for (Parameter par : c1.getAddedSubList()) {
                                    try {
                                        mqtt.subscribe(std_ctx.getStudent().id + "/output/" + par.name, (String topic, MqttMessage message) -> {
                                            Map<String, String> c_par_vals = JSONB.fromJson(new String(message.getPayload()), new HashMap<String, String>() {
                                            }.getClass().getGenericSuperclass());
                                            Platform.runLater(() -> std_ctx.setParameterValue(par.name, c_par_vals));
                                        });
                                    } catch (MqttException ex) {
                                        LOG.log(Level.SEVERE, null, ex);
                                    }
                                }
                                // we unsubscribe from the removed parameters..
                                for (Parameter par : c1.getRemoved()) {
                                    try {
                                        mqtt.unsubscribe(student_id + "/output/" + par.name);
                                    } catch (MqttException ex) {
                                        LOG.log(Level.SEVERE, null, ex);
                                    }
                                }
                            }
                        });
                        if (std_ctx.isOnline()) {
                            // we add the current student's parameters..
                            // notice that in case the student is offline, the parameters will be added by the subscription to the student's output..
                            std_ctx.parameterTypesProperty().addAll(std_ctx.getStudent().par_types.values());
                        }
                        // we subscribe to the student's output..
                        mqtt.subscribe(std_ctx.getStudent().id + "/output", (String topic, MqttMessage message) -> {
                            Message m = JSONB.fromJson(new String(message.getPayload()), Message.class);
                            switch (m.message_type) {
                                case NewParameter:
                                    NewParameter new_parameter = JSONB.fromJson(new String(message.getPayload()), NewParameter.class);
                                    Platform.runLater(() -> std_ctx.parameterTypesProperty().add(new_parameter.parameter));
                                    break;
                                case LostParameter:
                                    LostParameter lost_parameter = JSONB.fromJson(new String(message.getPayload()), LostParameter.class);
                                    Platform.runLater(() -> std_ctx.parameterTypesProperty().remove(std_ctx.getParameter(lost_parameter.name)));
                                    break;
                                case Answer:
                                    break;
                                default:
                                    throw new AssertionError(m.message_type.name());
                            }
                        });
                        for (Map.Entry<String, Map<String, String>> par_val : std_ctx.getStudent().par_values.entrySet()) {
                            std_ctx.setParameterValue(par_val.getKey(), par_val.getValue());
                        }
                    } catch (MqttException ex) {
                        LOG.log(Level.SEVERE, null, ex);
                    }
                    id_students.put(std_ctx.getStudent().id, std_ctx);
                }
                for (StudentContext std_ctx : c.getRemoved()) {
                    try {
                        mqtt.unsubscribe(std_ctx.getStudent().id + "/output/on-line");
                        std_ctx.parameterTypesProperty().clear();
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
        par_vals.get(par_name).get(sub_par).value.set(value);
        Map<String, String> val = new HashMap<>();
        for (Map.Entry<String, ParameterValue> v_val : par_vals.get(par_name).entrySet()) {
            val.put(v_val.getKey(), v_val.getValue().value.get());
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

    public void answerQuestion(QuestionEvent event, int answer) {
        Form form = new Form();
        form.param("lesson_id", Long.toString(event.lesson_id));
        form.param("question_id", Long.toString(event.event_id));
        form.param("answer_id", Long.toString(answer));
        target.path("answer_question").request().put(Entity.form(form));
    }

    public void addTeacher(User teacher) {
        Form form = new Form();
        form.param("student_id", Long.toString(user.get().id));
        form.param("teacher_id", Long.toString(teacher.id));
        target.path("add_teacher").request(MediaType.APPLICATION_JSON).put(Entity.form(form));
        teachers.add(new TeacherContext(teacher));
    }

    public void removeTeacher(TeacherContext tch_ctx) {
        Form form = new Form();
        form.param("student_id", Long.toString(user.get().id));
        form.param("teacher_id", Long.toString(tch_ctx.getTeacher().id));
        target.path("remove_teacher").request(MediaType.APPLICATION_JSON).put(Entity.form(form));
        teachers.remove(tch_ctx);
    }

    public ObservableList<TeacherContext> teachersProperty() {
        return teachers;
    }

    public ObservableList<LessonModel> modelsProperty() {
        return models;
    }

    public void addLesson(String lesson_name, LessonModel model, Map<String, Long> roles) {
        // we create a new lesson..
        NewLessonRequest new_lesson;
        Lesson lesson;
        if (model.id != null) {
            new_lesson = new NewLessonRequest(user.get().id, lesson_name, model.id, roles);
            lesson = target.path("new_lesson_by_model_id").request(MediaType.APPLICATION_JSON).post(Entity.json(new_lesson), Lesson.class);
        } else {
            new_lesson = new NewLessonRequest(user.get().id, lesson_name, model, roles);
            lesson = target.path("new_lesson_by_model").request(MediaType.APPLICATION_JSON).post(Entity.json(new_lesson), Lesson.class);
        }
        teaching_lessons.add(new TeachingLessonContext(lesson, model));

        // we solve the new lesson..
        Form form = new Form();
        form.param("lesson_id", Long.toString(lesson.id));
        target.path("solve_lesson").request().put(Entity.form(form));
    }

    public void removeLesson(TeachingLessonContext l_ctx) {
        target.path("lessons").path(Long.toString(l_ctx.getLesson().id)).request().delete();
        teaching_lessons.remove(l_ctx);
    }

    public ObservableList<TeachingLessonContext> teachingLessonsProperty() {
        return teaching_lessons;
    }

    public void setTime(Lesson lesson, TokenRow row, long time) {
        Form form = new Form();
        form.param("lesson_id", Long.toString(lesson.id));
        form.param("token_id", Integer.toString(row.getId()));
        form.param("time", Long.toString(time));
        target.path("set_time").request().put(Entity.form(form));
    }

    public void play(Lesson lesson) {
        Form form = new Form();
        form.param("lesson_id", Long.toString(lesson.id));
        target.path("play").request().put(Entity.form(form));
    }

    public void pause(Lesson lesson) {
        Form form = new Form();
        form.param("lesson_id", Long.toString(lesson.id));
        target.path("pause").request().put(Entity.form(form));
    }

    public void stop(Lesson lesson) {
        Form form = new Form();
        form.param("lesson_id", Long.toString(lesson.id));
        target.path("stop").request().put(Entity.form(form));
    }

    public void goTo(Lesson lesson, long time) {
        Form form = new Form();
        form.param("lesson_id", Long.toString(lesson.id));
        form.param("time", Long.toString(time));
        target.path("go_to").request().put(Entity.form(form));
    }

    public StudentContext getStudentContext(long id) {
        return id_students.get(id);
    }

    public ObservableList<StudentContext> studentsProperty() {
        return students;
    }

    public void login(String email, String password) {
        Credentials credentials = new Credentials(email, password);
        InitResponse init = target.path("login").request(MediaType.APPLICATION_JSON).post(Entity.json(credentials), InitResponse.class);
        init.user.par_types = load_pars();
        init.user.par_values = load_par_vals();
        user.set(init.user);

        // we add the following lessons..
        init.following_lessons.forEach(l -> following_lessons.add(new FollowingLessonContext(l)));

        // we add the teachers..
        init.teachers.forEach(t -> teachers.add(new TeacherContext(t)));

        // we add the available models..
        models.addAll(init.models);

        // we add the teaching lessons..
        init.teaching_lessons.forEach(l -> teaching_lessons.add(new TeachingLessonContext(l, init.models.stream().filter(m -> m.id.equals(l.model)).findAny().get())));

        // we add the students..
        init.students.forEach(s -> students.add(new StudentContext(s)));
    }

    public void logout() {
        user.set(null);
    }

    public void newUser(String email, String password, String first_name, String last_name) {
        NewUserRequest new_user = new NewUserRequest(email, password, first_name, last_name);
        User u = target.path("new_user").request(MediaType.APPLICATION_JSON).post(Entity.json(new_user), User.class);
        u.par_types = load_pars();
        u.par_values = load_par_vals();
        user.set(u);
    }

    public Collection<User> findUsers(String search_string) {
        return target.path("find_users").path(search_string).request(MediaType.APPLICATION_JSON).get(new GenericType<ArrayList<User>>() {
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
        private final ObservableList<ParUpdate> updates = FXCollections.observableArrayList();

        ParameterValue(String name, String value) {
            this.name = new SimpleStringProperty(name);
            this.value = new SimpleStringProperty(value);
            this.updates.add(new ParUpdate(System.currentTimeMillis(), value));
            this.value.addListener((ObservableValue<? extends String> observable, String oldValue, String newValue) -> updates.add(new ParUpdate(System.currentTimeMillis(), newValue)));
        }

        public StringProperty nameProperty() {
            return name;
        }

        public StringProperty valueProperty() {
            return value;
        }

        public ObservableList<ParUpdate> updatesProperty() {
            return updates;
        }
    }

    public static class ParUpdate {

        public final long time;
        public final String new_value;

        public ParUpdate(long time, String new_value) {
            this.time = time;
            this.new_value = new_value;
        }
    }
}
