/*
 * Copyright (C) 2018 Riccardo De Benedictis
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

import it.cnr.istc.ale.api.Lesson;
import it.cnr.istc.ale.api.LessonState;
import static it.cnr.istc.ale.api.LessonState.Stopped;
import it.cnr.istc.ale.api.model.LessonModel;
import it.cnr.istc.ale.server.solver.LessonManager;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.eclipse.paho.client.mqttv3.MqttException;

/**
 *
 * @author Riccardo De Benedictis
 */
public class LessonContext {

    private static final Logger LOG = Logger.getLogger(LessonContext.class.getName());
    private final Lesson lesson;
    private final LessonModel model;
    private final LessonManager manager;
    private LessonState state = Stopped;

    public LessonContext(Lesson lesson, LessonModel model, LessonManager manager) {
        this.lesson = lesson;
        this.model = model;
        this.manager = manager;
    }

    public Lesson getLesson() {
        return lesson;
    }

    public LessonModel getModel() {
        return model;
    }

    public LessonManager getManager() {
        return manager;
    }

    public LessonState getState() {
        return state;
    }

    public void setState(LessonState state) {
        this.state = state;
        try {
            Context.getContext().mqtt.publish(lesson.getTeacherId() + "/input/lesson-" + lesson.getId() + "/state", state.toString().getBytes(), 1, true);
        } catch (MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }
}
