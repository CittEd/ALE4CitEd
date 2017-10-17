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

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import it.cnr.istc.ale.api.Lesson;
import it.cnr.istc.ale.api.Parameter;
import it.cnr.istc.ale.api.User;
import it.cnr.istc.ale.api.messages.NewStudent;
import it.cnr.istc.ale.api.messages.LostStudent;
import it.cnr.istc.ale.api.messages.Event;
import it.cnr.istc.ale.api.messages.LostParameter;
import it.cnr.istc.ale.api.messages.Message;
import it.cnr.istc.ale.api.messages.NewParameter;
import java.io.IOException;
import java.util.ArrayList;
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
    private final Map<String, Parameter> u_parameter_types = new HashMap<>();
    private final Map<String, Map<String, StringProperty>> u_parameter_values = new HashMap<>();
    private final ObservableList<ParameterValue> u_par_values = FXCollections.observableArrayList();
    private final Map<Long, BooleanProperty> online_users = new HashMap<>();
    private final Map<Long, Map<String, Parameter>> parameter_types = new HashMap<>();
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
                mqtt.subscribe(u.getId() + "/input", (String topic, MqttMessage message) -> {
                    Message m = mapper.readValue(message.getPayload(), Message.class);
                    if (m instanceof NewStudent) {
                        add_student(ur.get_user(((NewStudent) m).getStudentId()));
                    } else if (m instanceof LostStudent) {
                        remove_student(students.stream().filter(s -> s.getId() == ((LostStudent) m).getStudentId()).findAny().get());
                    }
                });
                for (User teacher : ur.get_teachers(u.getId())) {
                    add_teacher(teacher);
                }
                for (User student : ur.get_students(u.getId())) {
                    add_student(student);
                }

                Collection<Parameter> pars = mapper.readValue(getClass().getResourceAsStream("/parameters/types.json"), new TypeReference<Collection<Parameter>>() {
                });
                for (Parameter par : pars) {
                    u_parameter_types.put(par.getName(), par);
                    mqtt.publish(u.getId() + "/output", mapper.writeValueAsBytes(new NewParameter(par.getName(), par.getProperties())), 2, false);
                }
                Map<String, Map<String, String>> values = mapper.readValue(getClass().getResourceAsStream("/parameters/values.json"), new TypeReference<Map<String, Map<String, String>>>() {
                });
                for (Map.Entry<String, Map<String, String>> value : values.entrySet()) {
                    u_parameter_values.put(value.getKey(), new HashMap<>());
                    for (Map.Entry<String, String> val : value.getValue().entrySet()) {
                        if (u_parameter_values.get(value.getKey()).containsKey(val.getKey())) {
                            u_parameter_values.get(value.getKey()).get(val.getKey()).set(val.getValue());
                        } else {
                            SimpleStringProperty val_prop = new SimpleStringProperty(val.getValue());
                            u_parameter_values.get(value.getKey()).put(val.getKey(), val_prop);
                            u_par_values.add(new ParameterValue(value.getKey() + "." + val.getKey(), val_prop));
                        }
                    }
                    mqtt.publish(u.getId() + "/output/" + value.getKey(), mapper.writeValueAsBytes(value.getValue()), 2, true);
                }
            } catch (MqttException | IOException ex) {
                LOG.log(Level.SEVERE, null, ex);
            }
        } else {
            if (user.isNotNull().get()) {
                assert mqtt != null;
                assert mqtt.isConnected();
                try {
                    mqtt.unsubscribe(user.get().getId() + "/input");
                    for (User student : new ArrayList<>(students)) {
                        remove_student(student);
                    }
                    for (User teacher : new ArrayList<>(teachers)) {
                        remove_teacher(teacher);
                    }
                    mqtt.disconnect();
                    mqtt.close();
                } catch (MqttException ex) {
                    LOG.log(Level.SEVERE, null, ex);
                }
            }
            following_lessons.clear();
            lessons.clear();
            mqtt = null;
        }
        user.set(u);
    }

    public void add_teacher(User teacher) {
        try {
            online_users.put(teacher.getId(), new SimpleBooleanProperty());
            teachers.add(teacher);
            mqtt.subscribe(teacher.getId() + "/output/on-line", (String topic, MqttMessage message) -> {
                online_users.get(teacher.getId()).set(Boolean.parseBoolean(new String(message.getPayload())));
            });
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    public void remove_teacher(User teacher) {
        try {
            mqtt.unsubscribe(teacher.getId() + "/output/on-line");
            teachers.remove(teacher);
            online_users.remove(teacher.getId());
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    private void add_student(User student) {
        try {
            online_users.put(student.getId(), new SimpleBooleanProperty());
            students.add(student);
            mqtt.subscribe(student.getId() + "/output/on-line", (String topic, MqttMessage message) -> {
                online_users.get(student.getId()).set(Boolean.parseBoolean(new String(message.getPayload())));
            });

            par_values.put(student.getId(), FXCollections.observableArrayList());
            parameter_values.put(student.getId(), new LinkedHashMap<>());
            for (Map.Entry<String, Parameter> par_type : ur.get_parameter_types(student.getId()).entrySet()) {
                parameter_values.get(student.getId()).put(par_type.getKey(), new HashMap<>());
                mqtt.subscribe(student.getId() + "/output/" + par_type.getKey(), (String topic, MqttMessage message) -> {
                    Map<String, String> value = mapper.readValue(new String(message.getPayload()), new TypeReference<Map<String, String>>() {
                    });
                    for (Map.Entry<String, String> val : value.entrySet()) {
                        if (parameter_values.get(student.getId()).get(par_type.getKey()).containsKey(val.getKey())) {
                            parameter_values.get(student.getId()).get(par_type.getKey()).get(val.getKey()).set(val.getValue());
                        } else {
                            SimpleStringProperty val_prop = new SimpleStringProperty(val.getValue());
                            parameter_values.get(student.getId()).get(par_type.getKey()).put(val.getKey(), val_prop);
                            par_values.get(student.getId()).add(new ParameterValue(par_type.getKey() + "." + val.getKey(), val_prop));
                        }
                    }
                });
            }

            parameter_types.put(student.getId(), new LinkedHashMap<>());
            mqtt.subscribe(student.getId() + "/output", (String topic, MqttMessage message) -> {
                Message m = mapper.readValue(message.getPayload(), Message.class);
                if (m instanceof NewParameter) {
                    NewParameter np = (NewParameter) m;
                    parameter_types.get(student.getId()).put(np.getName(), new Parameter(np.getName(), np.getProperties()));
                    mqtt.subscribe(student.getId() + "/output/" + np.getName(), (String par_topic, MqttMessage par_value) -> {
                        Map<String, String> value = mapper.readValue(par_value.getPayload(), new TypeReference<Map<String, String>>() {
                        });
                        for (Map.Entry<String, String> val : value.entrySet()) {
                            if (parameter_values.get(student.getId()).get(np.getName()).containsKey(val.getKey())) {
                                parameter_values.get(student.getId()).get(np.getName()).get(val.getKey()).set(val.getValue());
                            } else {
                                SimpleStringProperty val_prop = new SimpleStringProperty(val.getValue());
                                parameter_values.get(student.getId()).get(np.getName()).put(val.getKey(), val_prop);
                                par_values.get(student.getId()).add(new ParameterValue(np.getName() + "." + val.getKey(), val_prop));
                            }
                        }
                    });
                } else if (m instanceof LostParameter) {
                    LostParameter lp = (LostParameter) m;
                    mqtt.unsubscribe(student.getId() + "/output/" + lp.getName());
                    parameter_types.get(student.getId()).remove(lp.getName());
                    parameter_values.get(student.getId()).remove(lp.getName());
                    par_values.get(student.getId()).clear();
                    for (Map.Entry<String, Map<String, StringProperty>> par_value : parameter_values.get(student.getId()).entrySet()) {
                        for (Map.Entry<String, StringProperty> sub_val : par_value.getValue().entrySet()) {
                            par_values.get(student.getId()).add(new ParameterValue(par_value.getKey() + "." + sub_val.getKey(), parameter_values.get(student.getId()).get(par_value.getKey()).get(sub_val.getKey())));
                        }
                    }
                }
            });
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    private void remove_student(User student) {
        try {
            mqtt.unsubscribe(student.getId() + "/output/on-line");
            mqtt.unsubscribe(student.getId() + "/output");
            for (String par : parameter_types.get(student.getId()).keySet()) {
                mqtt.unsubscribe(student.getId() + "/output/" + par);
            }
            students.remove(student);
            online_users.remove(student.getId());
            parameter_types.remove(student.getId());
            parameter_values.remove(student.getId());
        } catch (MqttException ex) {
            Logger.getLogger(Context.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    public void par_update(String name) {
        Map<String, String> val = new HashMap<>();
        for (Map.Entry<String, StringProperty> par_val : u_parameter_values.get(name).entrySet()) {
            val.put(par_val.getKey(), par_val.getValue().get());
        }
        try {
            mqtt.publish(user.get().getId() + "/output/" + name, mapper.writeValueAsBytes(val), 2, true);
        } catch (JsonProcessingException | MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    public Parameter getParameterType(String name) {
        return u_parameter_types.get(name);
    }

    public Map<String, StringProperty> getParameterValue(String name) {
        return u_parameter_values.get(name);
    }

    public ObservableList<ParameterValue> getParameterValues() {
        return u_par_values;
    }

    public Parameter getParameterType(long user_id, String name) {
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

        public StringProperty nameProperty() {
            return name;
        }

        public StringProperty valueProperty() {
            return value;
        }
    }
}
