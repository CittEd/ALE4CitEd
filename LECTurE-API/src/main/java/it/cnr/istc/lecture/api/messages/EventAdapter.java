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
package it.cnr.istc.lecture.api.messages;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonNumber;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonValue;
import javax.json.bind.adapter.JsonbAdapter;

/**
 *
 * @author Riccardo De Benedictis
 */
public class EventAdapter implements JsonbAdapter<Event, JsonObject> {

    @Override
    public JsonObject adaptToJson(Event obj) throws Exception {
        JsonObjectBuilder c_object = Json.createObjectBuilder();
        c_object.add("message_type", obj.message_type.name());
        c_object.add("event_type", obj.event_type.name());
        c_object.add("lesson_id", obj.lesson_id);
        c_object.add("event_id", obj.event_id);
        c_object.add("targets", Json.createArrayBuilder(obj.targets));
        c_object.add("time", obj.time);
        switch (obj.event_type) {
            case TextEvent:
                c_object.add("content", ((TextEvent) obj).content);
                break;
            case QuestionEvent:
                c_object.add("question", ((QuestionEvent) obj).question);
                c_object.add("answers", Json.createArrayBuilder(((QuestionEvent) obj).answers));
                if (((QuestionEvent) obj).answer != null) {
                    c_object.add("answer", ((QuestionEvent) obj).answer);
                }
                break;
            case URLEvent:
                c_object.add("content", ((URLEvent) obj).content);
                c_object.add("url", ((URLEvent) obj).url);
                break;
            default:
                throw new AssertionError(obj.event_type.name());
        }
        return c_object.build();
    }

    @Override
    public Event adaptFromJson(JsonObject obj) throws Exception {
        long lesson_id = obj.getJsonNumber("lesson_id").longValue();
        int event_id = obj.getInt("event_id");
        List<Long> targets = obj.getJsonArray("targets").stream().map(id -> ((JsonNumber) id).longValue()).collect(Collectors.toList());
        long time = obj.getJsonNumber("time").longValue();
        switch (Event.EventType.valueOf(obj.getString("event_type"))) {
            case TextEvent:
                return new TextEvent(lesson_id, event_id, targets, time, obj.getString("content"));
            case QuestionEvent:
                String question = obj.getString("question");
                JsonArray answers_array = obj.getJsonArray("answers");
                List<String> answers = new ArrayList<>(answers_array.size());
                for (JsonValue answer_value : answers_array) {
                    answers.add(answer_value.toString());
                }
                return new QuestionEvent(lesson_id, event_id, targets, time, question, answers, obj.containsKey("answer") ? obj.getInt("answer") : null);
            case URLEvent:
                return new URLEvent(lesson_id, event_id, targets, time, obj.getString("content"), obj.getString("url"));
            default:
                throw new AssertionError(Event.EventType.valueOf(obj.getString("event_type")).name());
        }
    }
}
