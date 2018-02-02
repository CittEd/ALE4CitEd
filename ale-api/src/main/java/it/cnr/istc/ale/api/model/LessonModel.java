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
import java.util.Collection;
import java.util.Collections;

/**
 *
 * @author Riccardo De Benedictis
 */
public class LessonModel {

    private final String name;
    private final Collection<String> roles;
    private final Collection<Event> model;
    private final Collection<String> events;
    private final Collection<Relation> relations;

    @JsonCreator
    public LessonModel(@JsonProperty("name") String name, @JsonProperty("roles") Collection<String> roles, @JsonProperty("model") Collection<Event> model, @JsonProperty("events") Collection<String> events, @JsonProperty("relations") Collection<Relation> relations) {
        this.name = name;
        this.roles = roles;
        this.model = model;
        this.events = events;
        this.relations = relations;
    }

    public String getName() {
        return name;
    }

    public Collection<String> getRoles() {
        return Collections.unmodifiableCollection(roles);
    }

    public Collection<Event> getModel() {
        return Collections.unmodifiableCollection(model);
    }

    public Collection<String> getEvents() {
        return Collections.unmodifiableCollection(events);
    }

    public Collection<Relation> getRelations() {
        return Collections.unmodifiableCollection(relations);
    }
}
