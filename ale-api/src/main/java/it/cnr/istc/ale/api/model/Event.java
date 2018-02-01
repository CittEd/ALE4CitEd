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
package it.cnr.istc.ale.api.model;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonSubTypes;
import com.fasterxml.jackson.annotation.JsonSubTypes.Type;
import com.fasterxml.jackson.annotation.JsonTypeInfo;
import java.util.Collection;
import java.util.Collections;

/**
 *
 * @author Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
 */
@JsonTypeInfo(
        use = JsonTypeInfo.Id.NAME,
        include = JsonTypeInfo.As.PROPERTY,
        property = "type")
@JsonSubTypes({
    @Type(value = Event.class, name = "core"),
    @Type(value = TextEvent.class, name = "text"),
    @Type(value = QuestionEvent.class, name = "question")})
public class Event {

    private final String name;
    private final String role;
    private final Condition trigger_condition;
    private final Condition execution_condition;
    private final Collection<String> events;
    private final Collection<Relation> relations;

    @JsonCreator
    public Event(@JsonProperty("name") String name, @JsonProperty("role") String role, @JsonProperty("triggerCondition") Condition trigger_condition, @JsonProperty("executionCondition") Condition execution_condition, @JsonProperty("events") Collection<String> events, @JsonProperty("relations") Collection<Relation> relations) {
        this.name = name;
        this.role = role;
        this.trigger_condition = trigger_condition;
        this.execution_condition = execution_condition;
        this.events = events;
        this.relations = relations;
    }

    public String getName() {
        return name;
    }

    public String getRole() {
        return role;
    }

    public Condition getTriggerCondition() {
        return trigger_condition;
    }

    public Condition getExecutionCondition() {
        return execution_condition;
    }

    public Collection<String> getEvents() {
        return Collections.unmodifiableCollection(events);
    }

    public Collection<Relation> getRelations() {
        return Collections.unmodifiableCollection(relations);
    }
}
