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
package it.cnr.istc.lecture.webapp.api;

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
    private final Map<String, Parameter> parameter_types;
    private final Map<String, Map<String, String>> parameter_values;

    @JsonbCreator
    public User(@JsonbProperty("id") long id, @JsonbProperty("email") String email, @JsonbProperty("firstName") String firstName, @JsonbProperty("lastName") String lastName, @JsonbProperty("parameterTypes") Map<String, Parameter> parameter_types, @JsonbProperty("parameterValues") Map<String, Map<String, String>> parameter_values) {
        this.id = id;
        this.email = email;
        this.firstName = firstName;
        this.lastName = lastName;
        this.parameter_types = parameter_types;
        this.parameter_values = parameter_values;
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

    public Map<String, Parameter> getParameterTypes() {
        return Collections.unmodifiableMap(parameter_types);
    }

    public Map<String, Map<String, String>> getParameterValues() {
        return Collections.unmodifiableMap(parameter_values);
    }
}
