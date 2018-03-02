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
package it.cnr.istc.lecture.webapp.api.model;

import java.util.Collection;
import java.util.Collections;
import javax.json.bind.annotation.JsonbCreator;
import javax.json.bind.annotation.JsonbProperty;

/**
 *
 * @author Riccardo De Benedictis
 */
public class EventTemplate {

    private final EventTemplateType type;
    private final String name;
    private final String role;
    private final Condition trigger_condition;
    private final Condition execution_condition;
    private final Collection<String> events;
    private final Collection<Relation> relations;

    @JsonbCreator
    public EventTemplate(@JsonbProperty("type") EventTemplateType type, @JsonbProperty("name") String name, @JsonbProperty("role") String role, @JsonbProperty("triggerCondition") Condition trigger_condition, @JsonbProperty("executionCondition") Condition execution_condition, @JsonbProperty("events") Collection<String> events, @JsonbProperty("relations") Collection<Relation> relations) {
        this.type = type;
        this.name = name;
        this.role = role;
        this.trigger_condition = trigger_condition;
        this.execution_condition = execution_condition;
        this.events = events;
        this.relations = relations;
    }

    public EventTemplateType getType() {
        return type;
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
