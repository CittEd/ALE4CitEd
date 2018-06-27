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
package it.cnr.istc.lecture.api.model;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import javax.json.bind.annotation.JsonbTypeAdapter;

/**
 *
 * @author Riccardo De Benedictis
 */
public class LessonModel {

    public Long id;
    public String name;
    public ArrayList<String> roles;
    @JsonbTypeAdapter(EventTemplateListAdapter.class)
    public ArrayList<EventTemplate> events;
    public ArrayList<String> ids;
    public ArrayList<Relation> relations;

    public LessonModel() {
    }

    public LessonModel(long id, String name, Collection<String> roles, Collection<EventTemplate> events, Collection<String> ids, Collection<Relation> relations) {
        this.id = id;
        this.name = name;
        this.roles = new ArrayList<>(roles);
        this.events = new ArrayList<>(events);
        this.ids = new ArrayList<>(ids);
        this.relations = new ArrayList<>(relations);
    }

    public Set<String> getTopics() {
        Set<String> topics = new HashSet<>();
        events.forEach(e -> topics.addAll(e.topics));
        return topics;
    }
}
