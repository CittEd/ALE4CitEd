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
package it.cnr.istc.lecture.desktopapp.api.messages;

import javax.json.bind.annotation.JsonbCreator;
import javax.json.bind.annotation.JsonbProperty;

/**
 *
 * @author Riccardo De Benedictis
 */
public abstract class Event extends Message {

    private final EventType event_type;
    private final long lesson_id;
    private final int event_id;
    private final String role;

    @JsonbCreator
    public Event(@JsonbProperty("eventType") EventType event_type, @JsonbProperty("lessonId") long lesson_id, @JsonbProperty("eventId") int event_id, @JsonbProperty("role") String role) {
        super(MessageType.Event);
        this.event_type = event_type;
        this.lesson_id = lesson_id;
        this.event_id = event_id;
        this.role = role;
    }

    public EventType getEventType() {
        return event_type;
    }

    public long getLessonId() {
        return lesson_id;
    }

    public int getEventId() {
        return event_id;
    }

    public String getRole() {
        return role;
    }

    public enum EventType {
        TextEvent, QuestionEvent, URLEvent
    }
}
