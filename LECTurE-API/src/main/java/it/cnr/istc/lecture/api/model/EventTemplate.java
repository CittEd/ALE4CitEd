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

/**
 *
 * @author Riccardo De Benedictis
 */
public class EventTemplate {

    public EventTemplateType type;
    public String name;
    public String role;
    public Condition trigger_condition;
    public Condition execution_condition;
    public ArrayList<String> ids;
    public ArrayList<Relation> relations;

    public EventTemplate() {
    }

    public EventTemplate(EventTemplateType type, String name, String role, Condition trigger_condition, Condition execution_condition, Collection<String> ids, Collection<Relation> relations) {
        this.type = type;
        this.name = name;
        this.role = role;
        this.trigger_condition = trigger_condition;
        this.execution_condition = execution_condition;
        this.ids = new ArrayList<>(ids);
        this.relations = new ArrayList<>(relations);
    }

    public enum EventTemplateType {
        EventTemplate, TextEventTemplate, URLEventTemplate, QuestionEventTemplate
    }
}
