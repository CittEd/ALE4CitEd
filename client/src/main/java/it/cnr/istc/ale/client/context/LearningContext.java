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

import it.cnr.istc.ale.api.Lesson;
import it.cnr.istc.ale.api.User;
import it.cnr.istc.ale.api.messages.Event;
import java.util.logging.Level;
import java.util.logging.Logger;
import javafx.beans.Observable;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import org.eclipse.paho.client.mqttv3.MqttException;
import org.eclipse.paho.client.mqttv3.MqttMessage;

/**
 *
 * @author Riccardo De Benedictis
 */
public class LearningContext {

    private static final Logger LOG = Logger.getLogger(LearningContext.class.getName());
    private final Context ctx;
    /**
     * The received events.
     */
    private final ObservableList<Event> events = FXCollections.observableArrayList();
    /**
     * The lessons followed as a student.
     */
    private final ObservableList<Lesson> lessons = FXCollections.observableArrayList();
    /**
     * The followed teachers.
     */
    private final ObservableList<User> teachers = FXCollections.observableArrayList((User u) -> new Observable[]{Context.getContext().connection_ctx.online_users.get(u.getId())});

    LearningContext(Context ctx) {
        this.ctx = ctx;
    }

    public ObservableList<Event> getEvents() {
        return events;
    }

    void addLesson(Lesson lesson) {
        lessons.add(lesson);
    }

    void removeLesson(Lesson lesson) {
        lessons.remove(lesson);
    }

    public ObservableList<Lesson> getLessons() {
        return lessons;
    }

    void addTeacher(User teacher) {
        try {
            ctx.connection_ctx.online_users.put(teacher.getId(), new SimpleBooleanProperty());
            teachers.add(teacher);
            ctx.mqtt.subscribe(teacher.getId() + "/output/on-line", (String topic, MqttMessage message) -> {
                ctx.connection_ctx.online_users.get(teacher.getId()).set(Boolean.parseBoolean(new String(message.getPayload())));
            });
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    void removeTeacher(User teacher) {
        try {
            ctx.mqtt.unsubscribe(teacher.getId() + "/output/on-line");
            teachers.remove(teacher);
            ctx.connection_ctx.online_users.remove(teacher.getId());
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    public ObservableList<User> getTeachers() {
        return teachers;
    }
}
