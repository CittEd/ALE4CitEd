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

/**
 *
 * @author Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
 */
public class TextEvent extends Event {

    private final String content;

    @JsonCreator
    public TextEvent(@JsonProperty("name") String name, @JsonProperty("role") String role, @JsonProperty("trConditions") Collection<Condition> trConditions, @JsonProperty("exConditions") Collection<Condition> exConditions, @JsonProperty("events") Collection<String> events, @JsonProperty("relations") Collection<Relation> relations, @JsonProperty("content") String content) {
        super(name, role, trConditions, exConditions, events, relations);
        this.content = content;
    }

    public String getContent() {
        return content;
    }
}
