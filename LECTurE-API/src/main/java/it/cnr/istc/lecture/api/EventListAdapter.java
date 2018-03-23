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
import it.cnr.istc.lecture.api.messages.EventAdapter;
import java.util.ArrayList;
import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonValue;
import javax.json.bind.adapter.JsonbAdapter;

/**
 *
 * @author Riccardo De Benedictis
 */
public class EventListAdapter implements JsonbAdapter<ArrayList<Event>, JsonArray> {

    private static final EventAdapter EVENT_ADAPTER = new EventAdapter();

    @Override
    public JsonArray adaptToJson(ArrayList<Event> obj) throws Exception {
        JsonArrayBuilder es_builder = Json.createArrayBuilder();
        for (Event event : obj) {
            es_builder.add(Json.createObjectBuilder(EVENT_ADAPTER.adaptToJson(event)));
        }
        return es_builder.build();
    }

    @Override
    public ArrayList<Event> adaptFromJson(JsonArray obj) throws Exception {
        ArrayList<Event> es = new ArrayList<>(obj.size());
        for (JsonValue e_value : obj) {
            es.add(EVENT_ADAPTER.adaptFromJson(e_value.asJsonObject()));
        }
        return es;
    }
}
