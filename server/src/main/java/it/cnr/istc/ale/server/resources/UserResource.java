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

import it.cnr.istc.ale.api.User;
import it.cnr.istc.ale.api.UserAPI;
import it.cnr.istc.ale.api.messages.NewParameter;
import it.cnr.istc.ale.api.messages.ParameterUpdate;
import it.cnr.istc.ale.server.App;
import it.cnr.istc.ale.server.Context;
import it.cnr.istc.ale.server.db.UserEntity;
import java.util.Collection;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import javax.persistence.EntityManager;
import javax.persistence.NoResultException;
import javax.persistence.RollbackException;
import javax.persistence.TypedQuery;
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
        LOG.log(Level.INFO, "new user: {0} {1} {2}", new String[]{email, first_name, last_name});
        try {
            EntityManager em = App.emf.createEntityManager();
            UserEntity ue = new UserEntity();
            ue.setEmail(email);
            ue.setPassword(password);
            ue.setFirstName(first_name);
            ue.setLastName(last_name);
            em.getTransaction().begin();
            em.persist(ue);
            em.getTransaction().commit();
            return new User(ue.getId(), ue.getFirstName(), ue.getLastName());
        } catch (RollbackException e) {
            LOG.log(Level.INFO, "new user", e);
            throw new WebApplicationException(e.getLocalizedMessage(), Response.Status.FORBIDDEN);
        }
    }

    @Override
    @GET
    @Path("get_user")
    @Produces(MediaType.APPLICATION_JSON)
    public User get_user(@QueryParam("user_id") long user_id) {
        LOG.log(Level.INFO, "get_user: {0}", user_id);
        UserEntity ue = App.emf.createEntityManager().find(UserEntity.class, user_id);
        return new User(ue.getId(), ue.getFirstName(), ue.getLastName());
    }

    @Override
    @GET
    @Path("is_online")
    @Produces(MediaType.APPLICATION_JSON)
    public boolean is_online(@QueryParam("user_id") long user_id) {
        return Context.getContext().is_online(user_id);
    }

    @Override
    @GET
    @Path("get_parameter_types")
    @Produces(MediaType.APPLICATION_JSON)
    public Map<String, NewParameter> get_parameter_types(@QueryParam("user_id") long user_id) {
        return Context.getContext().get_parameter_types(user_id);
    }

    @Override
    @GET
    @Path("get_parameter_values")
    @Produces(MediaType.APPLICATION_JSON)
    public Map<String, ParameterUpdate> get_parameter_values(@QueryParam("user_id") long user_id) {
        return Context.getContext().get_parameter_values(user_id);
    }

    @Override
    @GET
    @Path("find")
    @Produces(MediaType.APPLICATION_JSON)
    public Collection<User> find_users(@QueryParam("search_string") String search_string) {
        LOG.log(Level.INFO, "find: {0}", search_string);
        EntityManager em = App.emf.createEntityManager();
        TypedQuery<UserEntity> query = em.createQuery("SELECT u FROM UserEntity u WHERE u.first_name LIKE :search_string OR u.last_name LIKE :search_string", UserEntity.class);
        query.setParameter("search_string", search_string);
        return query.getResultList().stream().map(usr -> new User(usr.getId(), usr.getFirstName(), usr.getLastName())).collect(Collectors.toList());
    }

    @Override
    @POST
    @Path("login")
    @Produces(MediaType.APPLICATION_JSON)
    public User login(@FormParam("email") String email, @FormParam("password") String password) {
        LOG.log(Level.INFO, "login: {0}", email);
        EntityManager em = App.emf.createEntityManager();
        TypedQuery<UserEntity> query = em.createQuery("SELECT u FROM UserEntity u WHERE u.email = :email AND u.password = :password", UserEntity.class);
        query.setParameter("email", email);
        query.setParameter("password", password);
        try {
            UserEntity ue = query.getSingleResult();
            return new User(ue.getId(), ue.getFirstName(), ue.getLastName());
        } catch (NoResultException e) {
            throw new WebApplicationException(e.getLocalizedMessage(), Response.Status.UNAUTHORIZED);
        }
    }

    @Override
    @PUT
    @Path("add_teacher")
    public void add_teacher(@FormParam("user_id") long user_id, @FormParam("teacher_id") long teacher_id) {
        EntityManager em = App.emf.createEntityManager();
        em.getTransaction().begin();
        UserEntity student = em.find(UserEntity.class, user_id);
        UserEntity teacher = em.find(UserEntity.class, teacher_id);
        student.addTeacher(teacher);
        teacher.addStudent(student);
        em.persist(student);
        em.getTransaction().commit();
        Context.getContext().add_teacher(user_id, teacher_id);
    }

    @Override
    @PUT
    @Path("remove_teacher")
    public void remove_teacher(@FormParam("user_id") long user_id, @FormParam("teacher_id") long teacher_id) {
        EntityManager em = App.emf.createEntityManager();
        em.getTransaction().begin();
        UserEntity student = em.find(UserEntity.class, user_id);
        UserEntity teacher = em.find(UserEntity.class, teacher_id);
        student.removeTeacher(teacher);
        teacher.removeStudent(student);
        em.persist(student);
        em.getTransaction().commit();
        Context.getContext().remove_teacher(user_id, teacher_id);
    }

    @Override
    @GET
    @Path("get_students")
    @Produces(MediaType.APPLICATION_JSON)
    public Collection<User> get_students(@QueryParam("user_id") long user_id) {
        try {
            return App.emf.createEntityManager().find(UserEntity.class, user_id).getStudents().stream().map(st -> new User(st.getId(), st.getFirstName(), st.getLastName())).collect(Collectors.toList());
        } catch (NoResultException e) {
            throw new WebApplicationException(e.getLocalizedMessage(), Response.Status.NOT_FOUND);
        }
    }

    @Override
    @GET
    @Path("get_teachers")
    @Produces(MediaType.APPLICATION_JSON)
    public Collection<User> get_teachers(@QueryParam("user_id") long user_id) {
        try {
            return App.emf.createEntityManager().find(UserEntity.class, user_id).getTeachers().stream().map(st -> new User(st.getId(), st.getFirstName(), st.getLastName())).collect(Collectors.toList());
        } catch (NoResultException e) {
            throw new WebApplicationException(e.getLocalizedMessage(), Response.Status.NOT_FOUND);
        }
    }
}
