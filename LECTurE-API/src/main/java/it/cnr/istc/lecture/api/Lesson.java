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
package it.cnr.istc.lecture.api;

import it.cnr.istc.lecture.api.messages.Event;
import it.cnr.istc.lecture.api.messages.Token;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import javax.json.bind.annotation.JsonbCreator;
import javax.json.bind.annotation.JsonbProperty;

/**
 *
 * @author Riccardo De Benedictis
 */
public class Lesson {

    private final long id;
    private final long teacher_id;
    private final String name;
    private final LessonState state;
    private final long time;
    private final Long model;
    private final Map<String, Long> roles;
    private final Collection<Event> events;
    private final Collection<Token> tokens;

    @JsonbCreator
    public Lesson(@JsonbProperty("id") long id, @JsonbProperty("teacherId") long teacher_id, @JsonbProperty("name") String name, @JsonbProperty("state") LessonState state, @JsonbProperty("time") long time, @JsonbProperty("model") Long model, @JsonbProperty("roles") Map<String, Long> roles, @JsonbProperty("events") Collection<Event> events, @JsonbProperty("tokens") Collection<Token> tokens) {
        this.id = id;
        this.teacher_id = teacher_id;
        this.name = name;
        this.state = state;
        this.time = time;
        this.model = model;
        this.roles = roles;
        this.events = events;
        this.tokens = tokens;
    }

    public long getId() {
        return id;
    }

    public long getTeacherId() {
        return teacher_id;
    }

    public String getName() {
        return name;
    }

    public LessonState getState() {
        return state;
    }

    public long getTime() {
        return time;
    }

    public Long getModel() {
        return model;
    }

    public Map<String, Long> getRoles() {
        return Collections.unmodifiableMap(roles);
    }

    public Collection<Event> getEvents() {
        return Collections.unmodifiableCollection(events);
    }

    public Collection<Token> getTokens() {
        return Collections.unmodifiableCollection(tokens);
    }

    public enum LessonState {
        Running, Paused, Stopped
    }
}
