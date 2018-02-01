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
package it.cnr.istc.ale.api;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import java.util.Collections;
import java.util.Map;

/**
 *
 * @author Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
 */
public class Lesson {

    private final long id;
    private final long teacher_id;
    private final String name;
    private final Map<String, Long> roles;

    @JsonCreator
    public Lesson(@JsonProperty("id") long id, @JsonProperty("teacherId") long teacher_id, @JsonProperty("name") String name, @JsonProperty("roles") Map<String, Long> roles) {
        this.id = id;
        this.teacher_id = teacher_id;
        this.name = name;
        this.roles = roles;
    }

    public long getId() {
        return id;
    }

    public long getTeacherId() {
        return teacher_id;
    }

    public String getName() {
        return name;
    }

    public Map<String, Long> getRoles() {
        return Collections.unmodifiableMap(roles);
    }
}
