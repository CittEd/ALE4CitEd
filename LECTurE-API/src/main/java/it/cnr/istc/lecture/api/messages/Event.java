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
package it.cnr.istc.lecture.api.messages;

import java.util.ArrayList;
import java.util.Collection;

/**
 *
 * @author Riccardo De Benedictis
 */
public abstract class Event extends Message {

    public EventType event_type;
    public long lesson_id;
    public int event_id;
    public ArrayList<Long> targets;
    public long time;

    public Event() {
    }

    public Event(EventType event_type, long lesson_id, int event_id, Collection<Long> targets, long time) {
        super(MessageType.Event);
        this.event_type = event_type;
        this.lesson_id = lesson_id;
        this.event_id = event_id;
        if (targets != null) {
            this.targets = new ArrayList<>(targets);
        }
        this.time = time;
    }

    public enum EventType {
        TextEvent, QuestionEvent, URLEvent
    }
}
