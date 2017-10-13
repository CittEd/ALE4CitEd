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
package it.cnr.istc.ale.client;

import it.cnr.istc.ale.api.User;
import it.cnr.istc.ale.api.UserAPI;
import static it.cnr.istc.ale.client.Context.REST_URI;
import java.util.Collection;
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

    public UserResource(Client client) {
        this.client = client;
    }

    @Override
    public User new_user(String email, String password, String first_name, String last_name) {
        Form form = new Form();
        form.param("email", email);
        form.param("password", password);
        form.param("first_name", first_name);
        form.param("last_name", last_name);
        return client.target(REST_URI)
                .path("users")
                .path("new_user")
                .request(MediaType.APPLICATION_JSON)
                .post(Entity.form(form), User.class);
    }

    @Override
    public Collection<User> find_users(String search_string) {
        return client.target(REST_URI)
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
        return client.target(REST_URI)
                .path("users")
                .path("login")
                .request(MediaType.APPLICATION_JSON)
                .post(Entity.form(form), User.class);
    }

    @Override
    public void add_teacher(long user_id, long teacher_id) {
        Form form = new Form();
        form.param("user_id", Long.toString(user_id));
        form.param("teacher_id", Long.toString(teacher_id));
        client.target(REST_URI)
                .path("users")
                .path("add_teacher")
                .request(MediaType.APPLICATION_JSON)
                .put(Entity.form(form));
    }

    @Override
    public void remove_teacher(long user_id, long teacher_id) {
        Form form = new Form();
        form.param("user_id", Long.toString(user_id));
        form.param("remove_teacher", Long.toString(teacher_id));
        client.target(REST_URI)
                .path("users")
                .path("add_teacher")
                .request(MediaType.APPLICATION_JSON)
                .put(Entity.form(form));
    }

    @Override
    public Collection<User> get_teachers(long user_id) {
        return client.target(REST_URI)
                .path("users")
                .path("get_teachers")
                .queryParam("user_id", Long.toString(user_id))
                .request(MediaType.APPLICATION_JSON)
                .get(new GenericType<Collection<User>>() {
                });
    }

    @Override
    public Collection<User> get_students(long user_id) {
        return client.target(REST_URI)
                .path("users")
                .path("get_students")
                .queryParam("user_id", Long.toString(user_id))
                .request(MediaType.APPLICATION_JSON)
                .get(new GenericType<Collection<User>>() {
                });
    }
}
