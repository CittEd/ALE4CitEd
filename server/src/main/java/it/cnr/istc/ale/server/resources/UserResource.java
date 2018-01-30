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
package it.cnr.istc.ale.server.resources;

import it.cnr.istc.ale.api.Parameter;
import it.cnr.istc.ale.api.User;
import it.cnr.istc.ale.api.UserAPI;
import it.cnr.istc.ale.server.Context;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.persistence.NoResultException;
import javax.persistence.RollbackException;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

/**
 *
 * @author Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
 */
@Path("users")
public class UserResource implements UserAPI {

    private static final Logger LOG = Logger.getLogger(UserResource.class.getName());

    @Override
    @POST
    @Path("new_user")
    @Produces(MediaType.APPLICATION_JSON)
    public User new_user(@FormParam("email") String email, @FormParam("password") String password, @FormParam("first_name") String first_name, @FormParam("last_name") String last_name) {
        try {
            return Context.getContext().new_user(email, password, first_name, last_name);
        } catch (RollbackException e) {
            LOG.log(Level.WARNING, "new user", e);
            throw new WebApplicationException(e.getLocalizedMessage(), Response.Status.FORBIDDEN);
        }
    }

    @Override
    @GET
    @Path("get_user")
    @Produces(MediaType.APPLICATION_JSON)
    public User get_user(@QueryParam("user_id") long user_id) {
        try {
            return Context.getContext().get_user(user_id);
        } catch (NoResultException e) {
            throw new WebApplicationException(e.getLocalizedMessage(), Response.Status.NOT_FOUND);
        }
    }

    @Override
    @GET
    @Path("get_parameter_types")
    @Produces(MediaType.APPLICATION_JSON)
    public Map<String, Parameter> get_parameter_types(@QueryParam("student_id") long student_id) {
        Map<String, Parameter> types = Context.getContext().get_parameter_types(student_id);
        if (types == null) {
            LOG.log(Level.WARNING, "No parameter types for user {0}", student_id);
            return Collections.emptyMap();
        } else {
            return types;
        }
    }

    @Override
    @GET
    @Path("find")
    @Produces(MediaType.APPLICATION_JSON)
    public Collection<User> find_users(@QueryParam("search_string") String search_string) {
        return Context.getContext().find_users(search_string);
    }

    @Override
    @POST
    @Path("login")
    @Produces(MediaType.APPLICATION_JSON)
    public User login(@FormParam("email") String email, @FormParam("password") String password) {
        try {
            return Context.getContext().login(email, password);
        } catch (NoResultException e) {
            throw new WebApplicationException(e.getLocalizedMessage(), Response.Status.UNAUTHORIZED);
        }
    }

    @Override
    @PUT
    @Path("add_teacher")
    public void add_teacher(@FormParam("student_id") long student_id, @FormParam("teacher_id") long teacher_id) {
        Context.getContext().add_teacher(student_id, teacher_id);
    }

    @Override
    @PUT
    @Path("remove_teacher")
    public void remove_teacher(@FormParam("student_id") long student_id, @FormParam("teacher_id") long teacher_id) {
        Context.getContext().remove_teacher(student_id, teacher_id);
    }

    @Override
    @GET
    @Path("get_teachers")
    @Produces(MediaType.APPLICATION_JSON)
    public Collection<User> get_teachers(@QueryParam("student_id") long student_id) {
        try {
            return Context.getContext().get_teachers(student_id);
        } catch (NoResultException e) {
            throw new WebApplicationException(e.getLocalizedMessage(), Response.Status.NOT_FOUND);
        }
    }

    @Override
    @GET
    @Path("get_students")
    @Produces(MediaType.APPLICATION_JSON)
    public Collection<User> get_students(@QueryParam("teacher_id") long teacher_id) {
        try {
            return Context.getContext().get_students(teacher_id);
        } catch (NoResultException e) {
            throw new WebApplicationException(e.getLocalizedMessage(), Response.Status.NOT_FOUND);
        }
    }
}
