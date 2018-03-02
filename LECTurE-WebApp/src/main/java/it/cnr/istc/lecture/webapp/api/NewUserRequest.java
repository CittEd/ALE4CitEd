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
public class NewUserRequest {

    private final String email;
    private final String password;
    private final String first_name;
    private final String last_name;
    private final Map<String, Parameter> parameter_types;
    private final Map<String, Map<String, String>> parameter_values;

    @JsonbCreator
    public NewUserRequest(@JsonbProperty("email") String email, @JsonbProperty("password") String password, @JsonbProperty("firstName") String first_name, @JsonbProperty("lastName") String last_name, @JsonbProperty("parameterTypes") Map<String, Parameter> parameter_types, @JsonbProperty("parameterValues") Map<String, Map<String, String>> parameter_values) {
        this.email = email;
        this.password = password;
        this.first_name = first_name;
        this.last_name = last_name;
        this.parameter_types = parameter_types;
        this.parameter_values = parameter_values;
    }

    public String getEmail() {
        return email;
    }

    public String getPassword() {
        return password;
    }

    public String getFirstName() {
        return first_name;
    }

    public String getLastName() {
        return last_name;
    }

    public Map<String, Parameter> getParameterTypes() {
        return Collections.unmodifiableMap(parameter_types);
    }

    public Map<String, Map<String, String>> getParameterValues() {
        return Collections.unmodifiableMap(parameter_values);
    }
}
