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
package it.cnr.istc.lecture.api.model;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonValue;
import javax.json.bind.adapter.JsonbAdapter;

/**
 *
 * @author Riccardo De Benedictis
 */
public class EventTemplateListAdapter implements JsonbAdapter<ArrayList<EventTemplate>, JsonArray> {

    private static final ConditionAdapter CONDITION_ADAPTER = new ConditionAdapter();

    @Override
    public JsonArray adaptToJson(ArrayList<EventTemplate> obj) throws Exception {
        JsonArrayBuilder ets_builder = Json.createArrayBuilder();
        for (EventTemplate et : obj) {
            JsonObjectBuilder et_object = Json.createObjectBuilder();
            et_object.add("type", et.type.name());
            et_object.add("name", et.name);
            et_object.add("role", et.role);
            if (et.trigger_condition != null) {
                et_object.add("trigger_condition", Json.createObjectBuilder(CONDITION_ADAPTER.adaptToJson(et.trigger_condition)));
            }
            if (et.execution_condition != null) {
                et_object.add("execution_condition", Json.createObjectBuilder(CONDITION_ADAPTER.adaptToJson(et.execution_condition)));
            }
            JsonArrayBuilder ids_builder = Json.createArrayBuilder();
            for (String id : et.ids) {
                ids_builder.add(id);
            }
            et_object.add("ids", ids_builder);
            JsonArrayBuilder rels_builder = Json.createArrayBuilder();
            for (Relation rel : et.relations) {
                JsonObjectBuilder rel_builder = Json.createObjectBuilder();
                rel_builder.add("from", rel.from);
                rel_builder.add("to", rel.to);
                if (rel.lb != Double.NEGATIVE_INFINITY) {
                    rel_builder.add("lb", rel.lb);
                }
                if (rel.ub != Double.POSITIVE_INFINITY) {
                    rel_builder.add("ub", rel.ub);
                }
                rel_builder.add("unit", rel.unit.name());
            }
            et_object.add("relations", rels_builder);
            switch (et.type) {
                case EventTemplate:
                    break;
                case TextEventTemplate:
                    et_object.add("content", ((TextEventTemplate) et).content);
                    break;
                case URLEventTemplate:
                    et_object.add("content", ((URLEventTemplate) et).content);
                    et_object.add("url", ((URLEventTemplate) et).url);
                    break;
                case QuestionEventTemplate:
                    et_object.add("question", ((QuestionEventTemplate) et).question);
                    JsonArrayBuilder ans_builder = Json.createArrayBuilder();
                    for (QuestionEventTemplate.Answer answer : ((QuestionEventTemplate) et).answers) {
                        ans_builder.add(Json.createObjectBuilder().add("answer", answer.answer).add("event", answer.event));
                    }
                    et_object.add("answers", ans_builder);
                    break;
                default:
                    throw new AssertionError(et.type.name());
            }
            ets_builder.add(et_object);
        }
        return ets_builder.build();
    }

    @Override
    public ArrayList<EventTemplate> adaptFromJson(JsonArray obj) throws Exception {
        ArrayList<EventTemplate> ets = new ArrayList<>(obj.size());
        for (JsonValue et_value : obj) {
            JsonObject et_object = et_value.asJsonObject();
            String name = et_object.getString("name");
            String role = et_object.getString("role");
            Condition trigger_condition = et_object.containsKey("trigger_condition") && !et_object.isNull("trigger_condition") ? CONDITION_ADAPTER.adaptFromJson(et_object.getJsonObject("trigger_condition")) : null;
            Condition execution_condition = et_object.containsKey("execution_condition") && !et_object.isNull("execution_condition") ? CONDITION_ADAPTER.adaptFromJson(et_object.getJsonObject("execution_condition")) : null;
            List<String> ids = new ArrayList<>(et_object.getJsonArray("ids").size());
            for (JsonValue id : et_object.getJsonArray("ids")) {
                ids.add(id.toString());
            }
            List<Relation> relations = new ArrayList<>(et_object.getJsonArray("relations").size());
            for (JsonValue rel_val : et_object.getJsonArray("relations")) {
                JsonObject rel_obj = rel_val.asJsonObject();
                relations.add(new Relation(rel_obj.getString("from"), rel_obj.getString("to"), rel_obj.containsKey("lb") ? rel_obj.getJsonNumber("lb").longValue() : null, rel_obj.containsKey("ub") ? rel_obj.getJsonNumber("ub").longValue() : null, TimeUnit.valueOf(rel_obj.getString("unit"))));
            }
            switch (EventTemplate.EventTemplateType.valueOf(et_object.getString("type"))) {
                case EventTemplate:
                    ets.add(new EventTemplate(EventTemplate.EventTemplateType.EventTemplate, name, role, trigger_condition, execution_condition, ids, relations));
                    break;
                case TextEventTemplate:
                    ets.add(new TextEventTemplate(name, role, trigger_condition, execution_condition, ids, relations, et_object.getString("content")));
                    break;
                case URLEventTemplate:
                    ets.add(new URLEventTemplate(name, role, trigger_condition, execution_condition, ids, relations, et_object.getString("content"), et_object.getString("url")));
                    break;
                case QuestionEventTemplate:
                    JsonArray answers_array = et_object.getJsonArray("answers");
                    List<QuestionEventTemplate.Answer> answers = new ArrayList<>(answers_array.size());
                    for (JsonValue ans_value : answers_array) {
                        JsonObject ans_onject = ans_value.asJsonObject();
                        answers.add(new QuestionEventTemplate.Answer(ans_onject.getString("answer"), ans_onject.getString("event")));
                    }
                    ets.add(new QuestionEventTemplate(name, role, trigger_condition, execution_condition, ids, relations, et_object.getString("question"), answers));
                    break;
                default:
                    throw new AssertionError(EventTemplate.EventTemplateType.valueOf(et_object.getString("type")).name());
            }
        }
        return ets;
    }
}
