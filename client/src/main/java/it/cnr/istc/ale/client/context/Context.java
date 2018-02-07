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
package it.cnr.istc.ale.client.context;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import it.cnr.istc.ale.api.Lesson;
import it.cnr.istc.ale.api.Parameter;
import it.cnr.istc.ale.api.User;
import it.cnr.istc.ale.api.messages.HideEvent;
import it.cnr.istc.ale.api.messages.TokenUpdate;
import it.cnr.istc.ale.api.messages.LostStudent;
import it.cnr.istc.ale.api.messages.Message;
import it.cnr.istc.ale.api.messages.Token;
import it.cnr.istc.ale.api.messages.NewLesson;
import it.cnr.istc.ale.api.messages.NewParameter;
import it.cnr.istc.ale.api.messages.NewStudent;
import it.cnr.istc.ale.api.messages.QuestionEvent;
import it.cnr.istc.ale.api.messages.TextEvent;
import it.cnr.istc.ale.api.model.LessonModel;
import it.cnr.istc.ale.client.Config;
import it.cnr.istc.ale.client.context.UserContext.ParameterValue;
import it.cnr.istc.ale.client.resources.LessonResource;
import it.cnr.istc.ale.client.resources.UserResource;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javafx.application.Platform;
import javafx.beans.property.SimpleStringProperty;
import javafx.stage.Stage;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
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
    private static Context context;
    public static final ObjectMapper MAPPER = new ObjectMapper();
    private Stage stage;
    final Client client = ClientBuilder.newClient();
    final UserResource ur = new UserResource(client);
    final LessonResource lr = new LessonResource(client);
    MqttClient mqtt;
    final ConnectionContext connection_ctx = new ConnectionContext(this);
    final LearningContext learning_ctx = new LearningContext(this);
    final TeachingContext teaching_ctx = new TeachingContext(this);
    final UserContext user_ctx = new UserContext(this);

    public static Context getContext() {
        if (context == null) {
            context = new Context();
        }
        return context;
    }

    public Stage getStage() {
        return stage;
    }

    public void setStage(Stage stage) {
        this.stage = stage;
    }

    public ConnectionContext getConnectionContext() {
        return connection_ctx;
    }

    public LearningContext getLearningContext() {
        return learning_ctx;
    }

    public TeachingContext getTeachingContext() {
        return teaching_ctx;
    }

    public UserContext getUserContext() {
        return user_ctx;
    }

    public void login(String email, String password) {
        set_user(ur.login(email, password));
    }

    public void newUser(String email, String password, String first_name, String last_name) {
        set_user(ur.new_user(email, password, first_name, last_name));
    }

    private void set_user(User u) {
        assert u != null;
        assert user_ctx.user.isNull().get();
        assert mqtt == null;
        user_ctx.user.set(u);
        try {
            mqtt = new MqttClient("tcp://" + Config.getInstance().getParam(Config.Param.Host) + ":" + Config.getInstance().getParam(Config.Param.MQTTPort), String.valueOf(u.getId()), new MemoryPersistence());
            mqtt.setCallback(new MqttCallback() {
                @Override
                public void connectionLost(Throwable cause) {
                    LOG.log(Level.SEVERE, null, cause);
                    set_user(null);
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

            mqtt.subscribe(u.getId() + "/input", (String topic, MqttMessage message) -> {
                Message m = MAPPER.readValue(message.getPayload(), Message.class);
                if (m instanceof NewLesson) {
                    // a new lesson has been created for this student..
                    Platform.runLater(() -> learning_ctx.addLesson(((NewLesson) m).getLesson()));
                } else if (m instanceof NewStudent) {
                    // a new student has started to follow this user..
                    Platform.runLater(() -> teaching_ctx.addStudent(ur.get_user(((NewStudent) m).getStudentId())));
                } else if (m instanceof LostStudent) {
                    // a student does not follow this user anymore..
                    Platform.runLater(() -> teaching_ctx.removeStudent(teaching_ctx.getStudents().stream().filter(s -> s.getId() == ((LostStudent) m).getStudentId()).findAny().get()));
                } else if (m instanceof Token) {
                    // a new event has been created for a lesson of this teacher..
                    Platform.runLater(() -> teaching_ctx.newToken(((Token) m)));
                } else if (m instanceof TokenUpdate) {
                    // an event of a lesson of this teacher has been updated..
                    Platform.runLater(() -> teaching_ctx.updateToken(((TokenUpdate) m)));
                } else if (m instanceof TextEvent) {
                    // a text event has been received..
                    Platform.runLater(() -> learning_ctx.addEvent(((TextEvent) m)));
                } else if (m instanceof QuestionEvent) {
                    // a question event has been received..
                    Platform.runLater(() -> learning_ctx.addEvent(((QuestionEvent) m)));
                } else if (m instanceof HideEvent) {
                    // a question event has been received..
                    Platform.runLater(() -> learning_ctx.removeEvent(((HideEvent) m)));
                } else {
                    LOG.log(Level.WARNING, "Not supported yet: {0}", m);
                }
            });
            // the lessons followed as a student..
            lr.get_followed_lessons(u.getId()).stream().forEach(lesson -> {
                learning_ctx.addLesson(lesson);
                lr.get_events(lesson.getId()).forEach(e -> learning_ctx.addEvent(e));
            });
            // the lessons followed as a teacher..
            lr.get_lessons(u.getId()).stream().forEach(lesson -> {
                teaching_ctx.addLesson(lesson, lesson.getModel());
                lr.get_tokens(lesson.getId()).stream().forEach(tk -> teaching_ctx.newToken(tk));
            });
            // the followed teachers..
            ur.get_teachers(u.getId()).forEach(teacher -> learning_ctx.addTeacher(teacher));
            // the followed students..
            ur.get_students(u.getId()).forEach(student -> teaching_ctx.addStudent(student));

            Collection<Parameter> pars = MAPPER.readValue(getClass().getResourceAsStream("/parameters/types.json"), new TypeReference<Collection<Parameter>>() {
            });
            for (Parameter par : pars) {
                user_ctx.parameter_types.put(par.getName(), par);
                mqtt.publish(u.getId() + "/output", MAPPER.writeValueAsBytes(new NewParameter(par)), 1, false);
            }
            Map<String, Map<String, String>> values = MAPPER.readValue(getClass().getResourceAsStream("/parameters/values.json"), new TypeReference<Map<String, Map<String, String>>>() {
            });
            for (Map.Entry<String, Map<String, String>> value : values.entrySet()) {
                user_ctx.parameter_values.put(value.getKey(), new HashMap<>());
                for (Map.Entry<String, String> val : value.getValue().entrySet()) {
                    if (user_ctx.parameter_values.get(value.getKey()).containsKey(val.getKey())) {
                        user_ctx.parameter_values.get(value.getKey()).get(val.getKey()).set(val.getValue());
                    } else {
                        SimpleStringProperty val_prop = new SimpleStringProperty(val.getValue());
                        user_ctx.parameter_values.get(value.getKey()).put(val.getKey(), val_prop);
                        user_ctx.par_values.add(new ParameterValue(value.getKey() + "." + val.getKey(), val_prop));
                    }
                }
                mqtt.publish(u.getId() + "/output/" + value.getKey(), MAPPER.writeValueAsBytes(value.getValue()), 1, true);
            }
        } catch (MqttException | IOException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    public void logout() {
        if (user_ctx.user.isNotNull().get()) {
            assert mqtt != null;
            assert mqtt.isConnected();
            try {
                mqtt.unsubscribe(user_ctx.user.get().getId() + "/input");
                new ArrayList<>(teaching_ctx.getStudents()).stream().forEach(teacher -> teaching_ctx.removeStudent(teacher));
                new ArrayList<>(learning_ctx.getTeachers()).stream().forEach(teacher -> learning_ctx.removeTeacher(teacher));
                new ArrayList<>(teaching_ctx.getLessons()).stream().forEach(lesson -> teaching_ctx.removeLesson(lesson));
                new ArrayList<>(learning_ctx.getLessons()).stream().forEach(lesson -> learning_ctx.removeLesson(lesson));
                user_ctx.parameter_types.clear();
                user_ctx.parameter_values.clear();
                user_ctx.par_values.clear();
                mqtt.disconnect();
                mqtt.close();
            } catch (MqttException ex) {
                LOG.log(Level.SEVERE, null, ex);
            }
        }
        mqtt = null;
        user_ctx.user.set(null);
    }

    public void addTeacher(User teacher) {
        ur.add_teacher(Context.getContext().getUserContext().getUser().get().getId(), teacher.getId());
        learning_ctx.addTeacher(teacher);
    }

    public void removeTeacher(User teacher) {
        ur.remove_teacher(Context.getContext().getUserContext().getUser().get().getId(), teacher.getId());
        learning_ctx.removeTeacher(teacher);
    }

    public void newLesson(String lesson_name, LessonModel model, Map<String, Long> roles) {
        try {
            // we create the lesson..
            Lesson lesson = lr.new_lesson_by_model(user_ctx.user.get().getId(), lesson_name, MAPPER.writeValueAsString(model), MAPPER.writeValueAsString(roles));
            // we create the context for the lesson..
            teaching_ctx.addLesson(lesson, model);
            // once created the context, we can solve the lesson..
            lr.solve_lesson(lesson.getId());
        } catch (JsonProcessingException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    public void removeLesson(Lesson lesson) {
        lr.remove_lesson(lesson.getId());
    }

    public Collection<User> findUsers(String search_string) {
        return ur.find_users(search_string);
    }
}
