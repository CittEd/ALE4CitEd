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
import java.util.Collection;
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
public class ConditionAdapter implements JsonbAdapter<Condition, JsonObject> {

    private static final ConditionAdapter CONDITION_ADAPTER = new ConditionAdapter();

    @Override
    public JsonObject adaptToJson(Condition obj) throws Exception {
        JsonObjectBuilder c_object = Json.createObjectBuilder();
        c_object.add("type", obj.type.name());
        switch (obj.type) {
            case AndCondition:
                JsonArrayBuilder and_condition_builder = Json.createArrayBuilder();
                for (Condition condition : ((AndCondition) obj).conditions) {
                    and_condition_builder.add(Json.createObjectBuilder(CONDITION_ADAPTER.adaptToJson(condition)));
                }
                c_object.add("conditions", and_condition_builder);
                break;
            case OrCondition:
                JsonArrayBuilder or_condition_builder = Json.createArrayBuilder();
                for (Condition condition : ((OrCondition) obj).conditions) {
                    or_condition_builder.add(Json.createObjectBuilder(CONDITION_ADAPTER.adaptToJson(condition)));
                }
                c_object.add("conditions", or_condition_builder);
                break;
            case NotCondition:
                c_object.add("condition", Json.createObjectBuilder(CONDITION_ADAPTER.adaptToJson(((NotCondition) obj).condition)));
                break;
            case NumericCondition:
                c_object.add("numeric_condition_type", ((NumericCondition) obj).numeric_condition_type.name());
                c_object.add("variable", ((NumericCondition) obj).variable);
                c_object.add("value", ((NumericCondition) obj).value);
                break;
            case NominalCondition:
                c_object.add("variable", ((NominalCondition) obj).variable);
                c_object.add("value", ((NominalCondition) obj).value);
                break;
            default:
                throw new AssertionError(obj.type.name());
        }
        return c_object.build();
    }

    @Override
    public Condition adaptFromJson(JsonObject obj) throws Exception {
        switch (Condition.ConditionType.valueOf(obj.getString("type"))) {
            case AndCondition:
                JsonArray and_conditions_array = obj.getJsonArray("conditions");
                Collection<Condition> and_conditions = new ArrayList<>(and_conditions_array.size());
                for (JsonValue cond_value : and_conditions_array) {
                    and_conditions.add(CONDITION_ADAPTER.adaptFromJson(cond_value.asJsonObject()));
                }
                return new AndCondition(and_conditions);
            case OrCondition:
                JsonArray or_conditions_array = obj.getJsonArray("conditions");
                Collection<Condition> or_conditions = new ArrayList<>(or_conditions_array.size());
                for (JsonValue cond_value : or_conditions_array) {
                    or_conditions.add(CONDITION_ADAPTER.adaptFromJson(cond_value.asJsonObject()));
                }
                return new OrCondition(or_conditions);
            case NotCondition:
                return new NotCondition(CONDITION_ADAPTER.adaptFromJson(obj.getJsonObject("condition")));
            case NumericCondition:
                return new NumericCondition(NumericCondition.NumericConditionType.valueOf(obj.getString("numeric_condition_type")), obj.getString("variable"), obj.getJsonNumber("value").doubleValue());
            case NominalCondition:
                return new NominalCondition(obj.getString("variable"), obj.getString("value"));
            default:
                throw new AssertionError(Condition.ConditionType.valueOf(obj.getString("type")).name());
        }
    }
}
