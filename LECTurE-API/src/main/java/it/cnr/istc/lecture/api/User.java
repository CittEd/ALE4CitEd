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

import java.util.Collections;
import java.util.Map;
import javax.json.bind.annotation.JsonbCreator;
import javax.json.bind.annotation.JsonbProperty;

/**
 *
 * @author Riccardo De Benedictis
 */
public class User {

    private final long id;
    private final String email;
    private final String firstName;
    private final String lastName;
    private final Map<String, Parameter> par_types;
    private final Map<String, Map<String, String>> par_values;

    @JsonbCreator
    public User(@JsonbProperty("id") long id, @JsonbProperty("email") String email, @JsonbProperty("firstName") String firstName, @JsonbProperty("lastName") String lastName, @JsonbProperty("parTypes") Map<String, Parameter> par_types, @JsonbProperty("parValues") Map<String, Map<String, String>> par_values) {
        this.id = id;
        this.email = email;
        this.firstName = firstName;
        this.lastName = lastName;
        this.par_types = par_types;
        this.par_values = par_values;
    }

    public long getId() {
        return id;
    }

    public String getEmail() {
        return email;
    }

    public String getFirstName() {
        return firstName;
    }

    public String getLastName() {
        return lastName;
    }

    public Map<String, Parameter> getParTypes() {
        return Collections.unmodifiableMap(par_types);
    }

    public Map<String, Map<String, String>> getParValues() {
        return Collections.unmodifiableMap(par_values);
    }
}
