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

import com.fasterxml.jackson.core.type.TypeReference;
import it.cnr.istc.ale.api.Lesson;
import it.cnr.istc.ale.api.LessonState;
import it.cnr.istc.ale.api.Parameter;
import it.cnr.istc.ale.api.User;
import it.cnr.istc.ale.api.messages.TokenUpdate;
import it.cnr.istc.ale.api.messages.LostParameter;
import it.cnr.istc.ale.api.messages.Message;
import it.cnr.istc.ale.api.messages.Token;
import it.cnr.istc.ale.api.messages.NewParameter;
import it.cnr.istc.ale.api.model.LessonModel;
import static it.cnr.istc.ale.client.context.Context.MAPPER;
import it.cnr.istc.ale.client.context.UserContext.ParameterValue;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javafx.application.Platform;
import javafx.beans.Observable;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.LongProperty;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleLongProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import org.eclipse.paho.client.mqttv3.MqttException;
import org.eclipse.paho.client.mqttv3.MqttMessage;

/**
 *
 * @author Riccardo De Benedictis
 */
public class TeachingContext {

    private static final Logger LOG = Logger.getLogger(TeachingContext.class.getName());
    private final Context ctx;
    /**
     * The lessons followed as a teacher.
     */
    private final ObservableList<Lesson> lessons = FXCollections.observableArrayList();
    /**
     * For each lesson, the lesson's context time.
     */
    private final Map<Long, LessonContext> lesson_context = new HashMap<>();
    /**
     * The following students.
     */
    private final ObservableList<User> students = FXCollections.observableArrayList((User u) -> new Observable[]{Context.getContext().connection_ctx.online_users.get(u.getId())});
    /**
     * The following students mapped by their id.
     */
    private final Map<Long, User> id_students = new HashMap<>();
    /**
     * For each user, the user's parameter types.
     */
    private final Map<Long, Map<String, Parameter>> user_parameter_types = new HashMap<>();
    /**
     * For each user, the user's parameter values.
     */
    private final Map<Long, Map<String, Map<String, StringProperty>>> user_parameter_values = new HashMap<>();
    /**
     * For each user, the user's parameter values as a list, to be displayed on
     * tables. Notice that each parameter can aggregate more than a single
     * value.
     */
    private final Map<Long, ObservableList<ParameterValue>> user_par_values = new HashMap<>();

    TeachingContext(Context ctx) {
        this.ctx = ctx;
    }

    void addLesson(Lesson lesson, LessonModel model) {
        lesson_context.put(lesson.getId(), new LessonContext(lesson, model));
        lessons.add(lesson);
        try {
            ctx.mqtt.subscribe(ctx.user_ctx.user.get().getId() + "/input/lesson-" + lesson.getId() + "/time", (String topic, MqttMessage message) -> {
                Platform.runLater(() -> lesson_context.get(lesson.getId()).time.setValue(Long.parseLong(new String(message.getPayload()))));
            });
            ctx.mqtt.subscribe(ctx.user_ctx.user.get().getId() + "/input/lesson-" + lesson.getId() + "/state", (String topic, MqttMessage message) -> {
                Platform.runLater(() -> lesson_context.get(lesson.getId()).state.setValue(LessonState.valueOf(new String(message.getPayload()))));
            });
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    void removeLesson(Lesson lesson) {
        try {
            ctx.mqtt.unsubscribe(ctx.user_ctx.user.get().getId() + "/input/lesson-" + lesson.getId() + "/time");
            lessons.remove(lesson);
            lesson_context.remove(lesson.getId());
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    public ObservableList<Lesson> getLessons() {
        return lessons;
    }

    public LongProperty getLessonTime(Lesson lesson) {
        return lesson_context.get(lesson.getId()).time;
    }

    public ObjectProperty<LessonState> getLessonState(Lesson lesson) {
        return lesson_context.get(lesson.getId()).state;
    }

    public LessonModel getLessonModel(Lesson lesson) {
        return lesson_context.get(lesson.getId()).model;
    }

    void newToken(Token tk) {
        lesson_context.get(tk.getLessonId()).tokens.add(new TokenRow(tk.getId(), lesson_context.get(tk.getLessonId()).time, tk.getTime(), tk.getRefEvent()));
    }

    void updateToken(TokenUpdate tk_update) {
        lesson_context.get(tk_update.getLessonId()).getToken(tk_update.getId()).time.set(tk_update.getTime());
    }

    public ObservableList<TokenRow> getTokens(Lesson lesson) {
        return lesson_context.get(lesson.getId()).tokens;
    }

    public void start(Lesson lesson) {
        ctx.lr.start_lesson(lesson.getId());
    }

    public void pause(Lesson lesson) {
        ctx.lr.pause_lesson(lesson.getId());
    }

    public void stop(Lesson lesson) {
        ctx.lr.stop_lesson(lesson.getId());
    }

    public void goTo(Lesson lesson, long time) {
        ctx.lr.go_at(lesson.getId(), time);
    }

    void addStudent(User student) {
        try {
            ctx.connection_ctx.online_users.put(student.getId(), new SimpleBooleanProperty());
            students.add(student);
            id_students.put(student.getId(), student);
            ctx.mqtt.subscribe(student.getId() + "/output/on-line", (String topic, MqttMessage message) -> {
                Platform.runLater(() -> ctx.connection_ctx.online_users.get(student.getId()).set(Boolean.parseBoolean(new String(message.getPayload()))));
            });

            user_parameter_types.put(student.getId(), new LinkedHashMap<>());
            user_parameter_values.put(student.getId(), new LinkedHashMap<>());
            user_par_values.put(student.getId(), FXCollections.observableArrayList());
            for (Map.Entry<String, Parameter> par_type : ctx.ur.get_parameter_types(student.getId()).entrySet()) {
                user_parameter_types.get(student.getId()).put(par_type.getKey(), par_type.getValue());
                user_parameter_values.get(student.getId()).put(par_type.getKey(), new HashMap<>());
                ctx.mqtt.subscribe(student.getId() + "/output/" + par_type.getKey(), (String topic, MqttMessage message) -> {
                    Map<String, String> value = MAPPER.readValue(new String(message.getPayload()), new TypeReference<Map<String, String>>() {
                    });
                    Platform.runLater(() -> {
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
                });
            }

            ctx.mqtt.subscribe(student.getId() + "/output", (String topic, MqttMessage message) -> {
                Message m = MAPPER.readValue(message.getPayload(), Message.class);
                if (m instanceof NewParameter) {
                    NewParameter np = (NewParameter) m;
                    Parameter par_type = np.getParameter();
                    user_parameter_types.get(student.getId()).put(par_type.getName(), par_type);
                    ctx.mqtt.subscribe(student.getId() + "/output/" + par_type.getName(), (String par_topic, MqttMessage par_value) -> {
                        Map<String, String> value = MAPPER.readValue(par_value.getPayload(), new TypeReference<Map<String, String>>() {
                        });
                        Platform.runLater(() -> {
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
                    });
                } else if (m instanceof LostParameter) {
                    LostParameter lp = (LostParameter) m;
                    ctx.mqtt.unsubscribe(student.getId() + "/output/" + lp.getName());
                    Platform.runLater(() -> {
                        user_parameter_types.get(student.getId()).remove(lp.getName());
                        user_parameter_values.get(student.getId()).remove(lp.getName());
                        user_par_values.get(student.getId()).clear();
                        for (Map.Entry<String, Map<String, StringProperty>> par_value : user_parameter_values.get(student.getId()).entrySet()) {
                            for (Map.Entry<String, StringProperty> sub_val : par_value.getValue().entrySet()) {
                                user_par_values.get(student.getId()).add(new ParameterValue(par_value.getKey() + "." + sub_val.getKey(), user_parameter_values.get(student.getId()).get(par_value.getKey()).get(sub_val.getKey())));
                            }
                        }
                    });
                }
            });
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    void removeStudent(User student) {
        try {
            ctx.mqtt.unsubscribe(student.getId() + "/output/on-line");
            ctx.mqtt.unsubscribe(student.getId() + "/output");
            for (String par : user_parameter_types.get(student.getId()).keySet()) {
                ctx.mqtt.unsubscribe(student.getId() + "/output/" + par);
            }
            students.remove(student);
            id_students.remove(student.getId());
            ctx.connection_ctx.online_users.remove(student.getId());
            user_parameter_types.remove(student.getId());
            user_parameter_values.remove(student.getId());
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    public User getStudent(long id) {
        return id_students.get(id);
    }

    public ObservableList<User> getStudents() {
        return students;
    }

    public ObservableList<ParameterValue> getParameterValues(long user_id) {
        return user_par_values.get(user_id);
    }

    public static class TokenRow {

        private final int id;
        private final BooleanProperty executed;
        private final LongProperty time;
        private final StringProperty name;

        private TokenRow(int id, LongProperty lesson_time, long time, String name) {
            this.id = id;
            this.executed = new SimpleBooleanProperty(false);
            this.time = new SimpleLongProperty(time);
            this.name = new SimpleStringProperty(name);
            executed.bind(lesson_time.greaterThanOrEqualTo(this.time));
        }

        public int getId() {
            return id;
        }

        public boolean getExecuted() {
            return executed.get();
        }

        public BooleanProperty executedProperty() {
            return executed;
        }

        public long getTime() {
            return time.get();
        }

        public LongProperty timeProperty() {
            return time;
        }

        public String getName() {
            return name.get();
        }

        public StringProperty nameProperty() {
            return name;
        }
    }
}
