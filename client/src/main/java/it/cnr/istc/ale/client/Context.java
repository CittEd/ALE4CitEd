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
package it.cnr.istc.ale.client;

import com.fasterxml.jackson.databind.ObjectMapper;
import it.cnr.istc.ale.api.Lesson;
import it.cnr.istc.ale.api.User;
import it.cnr.istc.ale.api.messages.ConnectionCreation;
import it.cnr.istc.ale.api.messages.ConnectionDestruction;
import it.cnr.istc.ale.api.messages.Event;
import it.cnr.istc.ale.api.messages.Message;
import java.util.logging.Level;
import java.util.logging.Logger;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.stage.Stage;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import org.eclipse.paho.client.mqttv3.MqttClient;
import org.eclipse.paho.client.mqttv3.MqttConnectOptions;
import org.eclipse.paho.client.mqttv3.MqttException;
import org.eclipse.paho.client.mqttv3.MqttMessage;
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence;

/**
 *
 * @author Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
 */
public class Context {

    private static final Logger LOG = Logger.getLogger(Context.class.getName());
    public static final String HOST = "localhost";
    public static final String SERVICE_PORT = "8080";
    public static final String REST_URI = "http://" + HOST + ":" + SERVICE_PORT;
    public static final String MQTT_PORT = "1883";
    private static Context context;
    private static final ObjectMapper mapper = new ObjectMapper();

    public static Context getContext() {
        if (context == null) {
            context = new Context();
        }
        return context;
    }
    private final Client client = ClientBuilder.newClient();
    private final UserResource ur = new UserResource(client);
    private final LessonResource lr = new LessonResource(client);
    private MqttClient mqtt;
    private Stage stage;
    private final ObjectProperty<User> user = new SimpleObjectProperty<>();
    private final ObservableList<Event> events = FXCollections.observableArrayList();
    private final ObservableList<Lesson> following_lessons = FXCollections.observableArrayList();
    private final ObservableList<User> teachers = FXCollections.observableArrayList();
    private final ObservableList<Lesson> lessons = FXCollections.observableArrayList();
    private final ObservableList<User> students = FXCollections.observableArrayList();

    private Context() {
    }

    public Client getClient() {
        return client;
    }

    public UserResource getUserResource() {
        return ur;
    }

    public Stage getStage() {
        return stage;
    }

    public void setStage(Stage stage) {
        this.stage = stage;
    }

    public ObjectProperty<User> getUser() {
        return user;
    }

    public ObservableList<Event> getEvents() {
        return events;
    }

    public ObservableList<Lesson> getFollowingLessons() {
        return following_lessons;
    }

    public ObservableList<User> getTeachers() {
        return teachers;
    }

    public ObservableList<Lesson> getLessons() {
        return lessons;
    }

    public ObservableList<User> getStudents() {
        return students;
    }

    public void login(String email, String password) {
        set_user(ur.login(email, password));
    }

    public void new_user(String email, String password, String first_name, String last_name) {
        set_user(ur.new_user(email, password, first_name, last_name));
    }

    public void logout() {
        set_user(null);
    }

    private void set_user(User u) {
        if (u != null) {
            assert user.isNull().get();
            assert mqtt == null;
            following_lessons.addAll(lr.get_followed_lessons(u.getId()));
            teachers.addAll(ur.get_teachers(u.getId()));
            lessons.addAll(lr.get_lessons(u.getId()));
            students.addAll(ur.get_students(u.getId()));
            try {
                mqtt = mqtt = new MqttClient("tcp://" + HOST + ":" + MQTT_PORT, String.valueOf(u.getId()), new MemoryPersistence());
                MqttConnectOptions options = new MqttConnectOptions();
                options.setCleanSession(false);
                mqtt.connect(options);
                mqtt.subscribe(String.valueOf(u.getId()) + "/input", (String topic, MqttMessage message) -> {
                    LOG.log(Level.INFO, "New message: {0}", message);
                    Message m = mapper.readValue(message.getPayload(), Message.class);
                    if (m instanceof ConnectionCreation) {
                        ConnectionCreation cc = (ConnectionCreation) m;
                        assert user.get().getId() == cc.getTeacherId();
                        students.add(ur.get_user(cc.getUserId()));
                    } else if (m instanceof ConnectionDestruction) {
                        ConnectionDestruction cd = (ConnectionDestruction) m;
                        assert user.get().getId() == cd.getTeacherId();
                        students.remove(students.stream().filter(s -> s.getId() == cd.getUserId()).findAny().get());
                    } else {
                        LOG.log(Level.WARNING, "Not supported yet.. {0}", new String(message.getPayload()));
                    }
                });
            } catch (MqttException ex) {
                throw new RuntimeException(ex.getMessage(), ex);
            }
        } else {
            assert user.isNotNull().get();
            assert mqtt != null;
            following_lessons.clear();
            teachers.clear();
            lessons.clear();
            students.clear();
            mqtt = null;
        }
        user.set(u);
    }

    public void add_teacher(User teacher) {
        ur.add_teacher(user.get().getId(), teacher.getId());
        teachers.add(teacher);
    }

    public void remove_teacher(User teacher) {
        ur.remove_teacher(user.get().getId(), teacher.getId());
        teachers.remove(teacher);
    }

    public void close() {
        if (user.isNotNull().get()) {
            try {
                mqtt.unsubscribe(String.valueOf(user.get().getId()) + "/input");
                mqtt.disconnect();
                mqtt.close();
            } catch (MqttException ex) {
                LOG.log(Level.SEVERE, null, ex);
            }
        }
    }
}
