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

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import it.cnr.istc.ale.api.Lesson;
import it.cnr.istc.ale.api.User;
import it.cnr.istc.ale.api.messages.NewConnection;
import it.cnr.istc.ale.api.messages.LostConnection;
import it.cnr.istc.ale.api.messages.Event;
import it.cnr.istc.ale.api.messages.LostParameter;
import it.cnr.istc.ale.api.messages.Message;
import it.cnr.istc.ale.api.messages.NewParameter;
import it.cnr.istc.ale.api.messages.ParameterUpdate;
import it.cnr.istc.ale.api.messages.UserOffline;
import it.cnr.istc.ale.api.messages.UserOnline;
import java.io.IOException;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javafx.beans.Observable;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
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
    private final Map<String, NewParameter> u_parameter_types = new HashMap<>();
    private final Map<String, Map<String, StringProperty>> u_parameter_values = new HashMap<>();
    private final ObservableList<ParameterValue> u_par_values = FXCollections.observableArrayList();
    private final Map<Long, BooleanProperty> online_users = new HashMap<>();
    private final Map<Long, Map<String, NewParameter>> parameter_types = new HashMap<>();
    private final Map<Long, Map<String, Map<String, StringProperty>>> parameter_values = new HashMap<>();
    private final Map<Long, ObservableList<ParameterValue>> par_values = new HashMap<>();
    private Stage stage;
    private final ObjectProperty<User> user = new SimpleObjectProperty<>();
    private final ObservableList<Event> events = FXCollections.observableArrayList();
    private final ObservableList<Lesson> following_lessons = FXCollections.observableArrayList();
    private final ObservableList<User> teachers = FXCollections.observableArrayList((User u) -> new Observable[]{online_users.get(u.getId())});
    private final ObservableList<Lesson> lessons = FXCollections.observableArrayList();
    private final ObservableList<User> students = FXCollections.observableArrayList((User u) -> new Observable[]{online_users.get(u.getId())});

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

    public BooleanProperty is_online(User user) {
        return online_users.get(user.getId());
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
            lessons.addAll(lr.get_lessons(u.getId()));
            try {
                mqtt = new MqttClient("tcp://" + HOST + ":" + MQTT_PORT, String.valueOf(u.getId()), new MemoryPersistence());
                MqttConnectOptions options = new MqttConnectOptions();
                options.setCleanSession(false);
                mqtt.connect(options);
                mqtt.subscribe(String.valueOf(u.getId()) + "/input", (String topic, MqttMessage message) -> {
                    LOG.log(Level.INFO, "New message: {0}", message);
                    Message m = mapper.readValue(message.getPayload(), Message.class);
                    if (m instanceof NewConnection) {
                        NewConnection cc = (NewConnection) m;
                        assert user.get().getId() == cc.getTeacherId();
                        students.add(ur.get_user(cc.getUserId()));
                    } else if (m instanceof LostConnection) {
                        LostConnection cd = (LostConnection) m;
                        assert user.get().getId() == cd.getTeacherId();
                        students.remove(students.stream().filter(s -> s.getId() == cd.getUserId()).findAny().get());
                    } else {
                        LOG.log(Level.WARNING, "Not supported yet.. {0}", new String(message.getPayload()));
                    }
                });
                for (User teacher : ur.get_teachers(u.getId())) {
                    teachers.add(teacher);
                    online_users.put(teacher.getId(), new SimpleBooleanProperty(ur.is_online(teacher.getId())));
                    mqtt.subscribe(String.valueOf(teacher.getId()) + "/output", (String topic, MqttMessage message) -> {
                        long id = Long.parseLong(topic.split("/")[0]);
                        Message m = mapper.readValue(message.getPayload(), Message.class);
                        if (m instanceof UserOnline) {
                            UserOnline uo = (UserOnline) m;
                            online_users.get(id).set(true);
                        } else if (m instanceof UserOffline) {
                            UserOffline uo = (UserOffline) m;
                            online_users.get(id).set(false);
                        }
                    });
                }
                for (User student : ur.get_students(u.getId())) {
                    students.add(student);
                    parameter_types.put(student.getId(), new LinkedHashMap<>());
                    for (Map.Entry<String, NewParameter> par_type : ur.get_parameter_types(student.getId()).entrySet()) {
                        parameter_types.get(student.getId()).put(par_type.getKey(), par_type.getValue());
                    }

                    par_values.put(student.getId(), FXCollections.observableArrayList());
                    parameter_values.put(student.getId(), new LinkedHashMap<>());
                    for (Map.Entry<String, ParameterUpdate> par_value : ur.get_parameter_values(student.getId()).entrySet()) {
                        parameter_values.get(student.getId()).put(par_value.getKey(), new LinkedHashMap<>());
                        for (Map.Entry<String, String> sub_val : par_value.getValue().getValue().entrySet()) {
                            SimpleStringProperty val_prop = new SimpleStringProperty(sub_val.getValue());
                            parameter_values.get(student.getId()).get(par_value.getKey()).put(sub_val.getKey(), val_prop);
                            par_values.get(student.getId()).add(new ParameterValue(par_value.getKey() + "." + sub_val.getKey(), val_prop));
                        }
                    }

                    online_users.put(student.getId(), new SimpleBooleanProperty(ur.is_online(student.getId())));

                    mqtt.subscribe(String.valueOf(student.getId()) + "/output", (String topic, MqttMessage message) -> {
                        long id = Long.parseLong(topic.split("/")[0]);
                        Message m = mapper.readValue(message.getPayload(), Message.class);
                        if (m instanceof NewParameter) {
                            NewParameter np = (NewParameter) m;
                            parameter_types.get(id).put(np.getName(), np);
                        } else if (m instanceof LostParameter) {
                            LostParameter lp = (LostParameter) m;
                            parameter_types.get(id).remove(lp.getName());
                            parameter_values.get(id).remove(lp.getName());
                            par_values.get(id).clear();
                            for (Map.Entry<String, Map<String, StringProperty>> par_value : parameter_values.get(id).entrySet()) {
                                for (Map.Entry<String, StringProperty> sub_val : par_value.getValue().entrySet()) {
                                    par_values.get(student.getId()).add(new ParameterValue(par_value.getKey() + "." + sub_val.getKey(), parameter_values.get(id).get(par_value.getKey()).get(sub_val.getKey())));
                                }
                            }
                        } else if (m instanceof ParameterUpdate) {
                            ParameterUpdate pu = (ParameterUpdate) m;
                            for (Map.Entry<String, String> sub_val : pu.getValue().entrySet()) {
                                parameter_values.get(id).get(pu.getParameter()).get(sub_val.getKey()).set(sub_val.getValue());
                            }
                        } else {
                            LOG.log(Level.WARNING, "Not supported yet.. {0}", new String(message.getPayload()));
                        }
                    });
                }

                Collection<NewParameter> par_types = mapper.readValue(getClass().getResourceAsStream("/sensors/types.json"), new TypeReference<Collection<NewParameter>>() {
                });
                for (NewParameter par_type : par_types) {
                    u_parameter_types.put(par_type.getName(), par_type);
                    mqtt.publish(String.valueOf(u.getId()) + "/output", new MqttMessage(mapper.writeValueAsBytes(par_type)));
                }
                Map<String, Map<String, String>> par_vals = mapper.readValue(getClass().getResourceAsStream("/sensors/types.json"), new TypeReference<Map<String, Map<String, String>>>() {
                });
                for (Map.Entry<String, Map<String, String>> par_val : par_vals.entrySet()) {
                    u_parameter_values.put(par_val.getKey(), new HashMap<>());
                    for (Map.Entry<String, String> sub_val : par_val.getValue().entrySet()) {
                        SimpleStringProperty val_prop = new SimpleStringProperty(sub_val.getValue());
                        u_parameter_values.get(par_val.getKey()).put(sub_val.getKey(), val_prop);
                        u_par_values.add(new ParameterValue(sub_val.getKey(), val_prop));
                    }
                    mqtt.publish(String.valueOf(u.getId()) + "/output", new MqttMessage(mapper.writeValueAsBytes(new ParameterUpdate(par_val.getKey(), par_val.getValue()))));
                }
            } catch (MqttException | IOException ex) {
                LOG.log(Level.SEVERE, null, ex);
            }
        } else {
            if (user.isNotNull().get()) {
                assert mqtt != null;
                try {
                    mqtt.unsubscribe(String.valueOf(user.get().getId()) + "/input");
                    for (User student : students) {
                        mqtt.unsubscribe(String.valueOf(student.getId()) + "/output");
                    }
                    mqtt.disconnect();
                    mqtt.close();
                } catch (MqttException ex) {
                    LOG.log(Level.SEVERE, null, ex);
                }
            }
            following_lessons.clear();
            teachers.clear();
            lessons.clear();
            students.clear();
            u_parameter_types.clear();
            u_parameter_values.clear();
            u_par_values.clear();
            online_users.clear();
            parameter_types.clear();
            parameter_values.clear();
            par_values.clear();
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

    public NewParameter getParameterType(String name) {
        return u_parameter_types.get(name);
    }

    public Map<String, StringProperty> getParameterValue(String name) {
        return u_parameter_values.get(name);
    }

    public ObservableList<ParameterValue> getParameterValues() {
        return u_par_values;
    }

    public NewParameter getParameterType(long user_id, String name) {
        return parameter_types.get(user_id).get(name);
    }

    public Map<String, StringProperty> getParameterValue(long user_id, String name) {
        return parameter_values.get(user_id).get(name);
    }

    public ObservableList<ParameterValue> getParameterValues(long user_id) {
        return par_values.get(user_id);
    }

    public static class ParameterValue {

        private final StringProperty name;
        private final StringProperty value;

        public ParameterValue(String name, StringProperty value) {
            this.name = new SimpleStringProperty(name);
            this.value = value;
        }
    }
}
