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
    private static Context context;
    public static final ObjectMapper MAPPER = new ObjectMapper();

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
    private final Map<String, Parameter> parameter_types = new HashMap<>();
    private final Map<String, Map<String, StringProperty>> parameter_values = new HashMap<>();
    private final ObservableList<ParameterValue> par_values = FXCollections.observableArrayList();
    private final Map<Long, BooleanProperty> online_users = new HashMap<>();
    private final Map<Long, Map<String, Parameter>> user_parameter_types = new HashMap<>();
    private final Map<Long, Map<String, Map<String, StringProperty>>> user_parameter_values = new HashMap<>();
    private final Map<Long, ObservableList<ParameterValue>> user_par_values = new HashMap<>();
    private Stage stage;
    private final ObjectProperty<User> user = new SimpleObjectProperty<>();
    private final ObservableList<Event> events = FXCollections.observableArrayList();
    private final ObservableList<Lesson> followed_lessons = FXCollections.observableArrayList();
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

    public LessonResource getLessonResource() {
        return lr;
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
        return followed_lessons;
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
            for (Lesson lesson : lr.get_followed_lessons(u.getId())) {
                add_followed_lesson(lesson);
            }
            for (Lesson lesson : lr.get_lessons(u.getId())) {
                add_lesson(lesson);
            }
            try {
                mqtt = new MqttClient("tcp://" + Config.getInstance().getParam(Config.Param.Host) + ":" + Config.getInstance().getParam(Config.Param.MQTTPort), String.valueOf(u.getId()), new MemoryPersistence());
                mqtt.setCallback(new MqttCallback() {
                    @Override
                    public void connectionLost(Throwable cause) {
                        LOG.log(Level.SEVERE, null, cause);
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
                options.setCleanSession(false);
                options.setAutomaticReconnect(true);
                mqtt.connect(options);
                mqtt.subscribe(u.getId() + "/input", (String topic, MqttMessage message) -> {
                    Message m = MAPPER.readValue(message.getPayload(), Message.class);
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

                Collection<Parameter> pars = MAPPER.readValue(getClass().getResourceAsStream("/parameters/types.json"), new TypeReference<Collection<Parameter>>() {
                });
                for (Parameter par : pars) {
                    parameter_types.put(par.getName(), par);
                    mqtt.publish(u.getId() + "/output", MAPPER.writeValueAsBytes(new NewParameter(par)), 1, false);
                }
                Map<String, Map<String, String>> values = MAPPER.readValue(getClass().getResourceAsStream("/parameters/values.json"), new TypeReference<Map<String, Map<String, String>>>() {
                });
                for (Map.Entry<String, Map<String, String>> value : values.entrySet()) {
                    parameter_values.put(value.getKey(), new HashMap<>());
                    for (Map.Entry<String, String> val : value.getValue().entrySet()) {
                        if (parameter_values.get(value.getKey()).containsKey(val.getKey())) {
                            parameter_values.get(value.getKey()).get(val.getKey()).set(val.getValue());
                        } else {
                            SimpleStringProperty val_prop = new SimpleStringProperty(val.getValue());
                            parameter_values.get(value.getKey()).put(val.getKey(), val_prop);
                            par_values.add(new ParameterValue(value.getKey() + "." + val.getKey(), val_prop));
                        }
                    }
                    mqtt.publish(u.getId() + "/output/" + value.getKey(), MAPPER.writeValueAsBytes(value.getValue()), 1, true);
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
            for (Lesson lesson : new ArrayList<>(followed_lessons)) {
                remove_followed_lesson(lesson);
            }
            for (Lesson lesson : new ArrayList<>(lessons)) {
                remove_lesson(lesson);
            }
            parameter_types.clear();
            parameter_values.clear();
            par_values.clear();
            mqtt = null;
        }
        user.set(u);
    }

    public void add_followed_lesson(Lesson lesson) {
        followed_lessons.add(lesson);
    }

    public void remove_followed_lesson(Lesson lesson) {
        followed_lessons.remove(lesson);
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

    public void add_lesson(Lesson lesson) {
        lessons.add(lesson);
    }

    public void remove_lesson(Lesson lesson) {
        lessons.remove(lesson);
    }

    private void add_student(User student) {
        try {
            online_users.put(student.getId(), new SimpleBooleanProperty());
            students.add(student);
            mqtt.subscribe(student.getId() + "/output/on-line", (String topic, MqttMessage message) -> {
                online_users.get(student.getId()).set(Boolean.parseBoolean(new String(message.getPayload())));
            });

            user_parameter_types.put(student.getId(), new LinkedHashMap<>());
            user_parameter_values.put(student.getId(), new LinkedHashMap<>());
            user_par_values.put(student.getId(), FXCollections.observableArrayList());
            for (Map.Entry<String, Parameter> par_type : ur.get_parameter_types(student.getId()).entrySet()) {
                user_parameter_types.get(student.getId()).put(par_type.getKey(), par_type.getValue());
                user_parameter_values.get(student.getId()).put(par_type.getKey(), new HashMap<>());
                mqtt.subscribe(student.getId() + "/output/" + par_type.getKey(), (String topic, MqttMessage message) -> {
                    Map<String, String> value = MAPPER.readValue(new String(message.getPayload()), new TypeReference<Map<String, String>>() {
                    });
                    for (Map.Entry<String, String> val : value.entrySet()) {
                        if (user_parameter_values.get(student.getId()).get(par_type.getKey()).containsKey(val.getKey())) {
                            user_parameter_values.get(student.getId()).get(par_type.getKey()).get(val.getKey()).set(val.getValue());
                        } else {
                            SimpleStringProperty val_prop = new SimpleStringProperty(val.getValue());
                            user_parameter_values.get(student.getId()).get(par_type.getKey()).put(val.getKey(), val_prop);
                            user_par_values.get(student.getId()).add(new ParameterValue(par_type.getKey() + "." + val.getKey(), val_prop));
                        }
                    }
                });
            }

            mqtt.subscribe(student.getId() + "/output", (String topic, MqttMessage message) -> {
                Message m = MAPPER.readValue(message.getPayload(), Message.class);
                if (m instanceof NewParameter) {
                    NewParameter np = (NewParameter) m;
                    Parameter par_type = np.getParameter();
                    user_parameter_types.get(student.getId()).put(par_type.getName(), par_type);
                    mqtt.subscribe(student.getId() + "/output/" + par_type.getName(), (String par_topic, MqttMessage par_value) -> {
                        Map<String, String> value = MAPPER.readValue(par_value.getPayload(), new TypeReference<Map<String, String>>() {
                        });
                        for (Map.Entry<String, String> val : value.entrySet()) {
                            if (user_parameter_values.get(student.getId()).get(par_type.getName()).containsKey(val.getKey())) {
                                user_parameter_values.get(student.getId()).get(par_type.getName()).get(val.getKey()).set(val.getValue());
                            } else {
                                SimpleStringProperty val_prop = new SimpleStringProperty(val.getValue());
                                user_parameter_values.get(student.getId()).get(par_type.getName()).put(val.getKey(), val_prop);
                                user_par_values.get(student.getId()).add(new ParameterValue(par_type.getName() + "." + val.getKey(), val_prop));
                            }
                        }
                    });
                } else if (m instanceof LostParameter) {
                    LostParameter lp = (LostParameter) m;
                    mqtt.unsubscribe(student.getId() + "/output/" + lp.getName());
                    user_parameter_types.get(student.getId()).remove(lp.getName());
                    user_parameter_values.get(student.getId()).remove(lp.getName());
                    user_par_values.get(student.getId()).clear();
                    for (Map.Entry<String, Map<String, StringProperty>> par_value : user_parameter_values.get(student.getId()).entrySet()) {
                        for (Map.Entry<String, StringProperty> sub_val : par_value.getValue().entrySet()) {
                            user_par_values.get(student.getId()).add(new ParameterValue(par_value.getKey() + "." + sub_val.getKey(), user_parameter_values.get(student.getId()).get(par_value.getKey()).get(sub_val.getKey())));
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
            for (String par : user_parameter_types.get(student.getId()).keySet()) {
                mqtt.unsubscribe(student.getId() + "/output/" + par);
            }
            students.remove(student);
            online_users.remove(student.getId());
            user_parameter_types.remove(student.getId());
            user_parameter_values.remove(student.getId());
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    public void par_update(String name) {
        Map<String, String> val = new HashMap<>();
        for (Map.Entry<String, StringProperty> par_val : parameter_values.get(name).entrySet()) {
            val.put(par_val.getKey(), par_val.getValue().get());
        }
        try {
            mqtt.publish(user.get().getId() + "/output/" + name, MAPPER.writeValueAsBytes(val), 1, true);
        } catch (JsonProcessingException | MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    public Parameter getParameterType(String name) {
        return parameter_types.get(name);
    }

    public Map<String, StringProperty> getParameterValue(String name) {
        return parameter_values.get(name);
    }

    public ObservableList<ParameterValue> getParameterValues() {
        return par_values;
    }

    public Parameter getParameterType(long user_id, String name) {
        return user_parameter_types.get(user_id).get(name);
    }

    public Map<String, StringProperty> getParameterValue(long user_id, String name) {
        return user_parameter_values.get(user_id).get(name);
    }

    public ObservableList<ParameterValue> getParameterValues(long user_id) {
        return user_par_values.get(user_id);
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
