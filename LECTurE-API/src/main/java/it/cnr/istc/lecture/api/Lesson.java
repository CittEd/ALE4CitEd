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
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 *
 * @author Riccardo De Benedictis
 */
public class Lesson {

    public long id;
    public long teacher_id;
    public String name;
    public LessonState state;
    public long time;
    public Long model;
    public HashMap<String, Long> roles;
    public ArrayList<Event> events;
    public ArrayList<Token> tokens;

    public Lesson() {
    }

    public Lesson(long id, long teacher_id, String name, LessonState state, long time, Long model, Map<String, Long> roles, Collection<Event> events, Collection<Token> tokens) {
        this.id = id;
        this.teacher_id = teacher_id;
        this.name = name;
        this.state = state;
        this.time = time;
        this.model = model;
        this.roles = new HashMap<>(roles);
        if (events != null) {
            this.events = new ArrayList<>(events);
        }
        if (tokens != null) {
            this.tokens = new ArrayList<>(tokens);
        }
    }

    public enum LessonState {
        Running, Paused, Stopped
    }
}
