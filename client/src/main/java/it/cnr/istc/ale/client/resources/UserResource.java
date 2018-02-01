/*
 * Copyright (C) 2018 Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
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
package it.cnr.istc.ale.client.resources;

import it.cnr.istc.ale.api.Parameter;
import it.cnr.istc.ale.api.User;
import it.cnr.istc.ale.api.UserAPI;
import it.cnr.istc.ale.client.Config;
import java.util.Collection;
import java.util.Map;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.Entity;
import javax.ws.rs.core.Form;
import javax.ws.rs.core.GenericType;
import javax.ws.rs.core.MediaType;

/**
 *
 * @author Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
 */
public class UserResource implements UserAPI {

    private final Client client;
    private final String rest_uri;

    public UserResource(Client client) {
        this.client = client;
        this.rest_uri = "http://" + Config.getInstance().getParam(Config.Param.Host) + ":" + Config.getInstance().getParam(Config.Param.ServicePort);
    }

    @Override
    public User new_user(String email, String password, String first_name, String last_name) {
        Form form = new Form();
        form.param("email", email);
        form.param("password", password);
        form.param("first_name", first_name);
        form.param("last_name", last_name);
        return client.target(rest_uri)
                .path("users")
                .path("new_user")
                .request(MediaType.APPLICATION_JSON)
                .post(Entity.form(form), User.class);
    }

    @Override
    public User get_user(long user_id) {
        return client.target(rest_uri)
                .path("users")
                .path("get_user")
                .queryParam("user_id", user_id)
                .request(MediaType.APPLICATION_JSON)
                .get(User.class);
    }

    @Override
    public Map<String, Parameter> get_parameter_types(long student_id) {
        return client.target(rest_uri)
                .path("users")
                .path("get_parameter_types")
                .queryParam("student_id", student_id)
                .request(MediaType.APPLICATION_JSON)
                .get(new GenericType<Map<String, Parameter>>() {
                });
    }

    @Override
    public Collection<User> find_users(String search_string) {
        return client.target(rest_uri)
                .path("users")
                .path("find")
                .queryParam("search_string", search_string)
                .request(MediaType.APPLICATION_JSON)
                .get(new GenericType<Collection<User>>() {
                });
    }

    @Override
    public User login(String email, String password) {
        Form form = new Form();
        form.param("email", email);
        form.param("password", password);
        return client.target(rest_uri)
                .path("users")
                .path("login")
                .request(MediaType.APPLICATION_JSON)
                .post(Entity.form(form), User.class);
    }

    @Override
    public void add_teacher(long student_id, long teacher_id) {
        Form form = new Form();
        form.param("student_id", Long.toString(student_id));
        form.param("teacher_id", Long.toString(teacher_id));
        client.target(rest_uri)
                .path("users")
                .path("add_teacher")
                .request(MediaType.APPLICATION_JSON)
                .put(Entity.form(form));
    }

    @Override
    public void remove_teacher(long student_id, long teacher_id) {
        Form form = new Form();
        form.param("student_id", Long.toString(student_id));
        form.param("teacher_id", Long.toString(teacher_id));
        client.target(rest_uri)
                .path("users")
                .path("remove_teacher")
                .request(MediaType.APPLICATION_JSON)
                .put(Entity.form(form));
    }

    @Override
    public Collection<User> get_teachers(long student_id) {
        return client.target(rest_uri)
                .path("users")
                .path("get_teachers")
                .queryParam("student_id", Long.toString(student_id))
                .request(MediaType.APPLICATION_JSON)
                .get(new GenericType<Collection<User>>() {
                });
    }

    @Override
    public Collection<User> get_students(long teacher_id) {
        return client.target(rest_uri)
                .path("users")
                .path("get_students")
                .queryParam("teacher_id", Long.toString(teacher_id))
                .request(MediaType.APPLICATION_JSON)
                .get(new GenericType<Collection<User>>() {
                });
    }
}
