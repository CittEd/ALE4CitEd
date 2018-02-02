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
package it.cnr.istc.ale.server;

import com.fasterxml.jackson.core.JsonProcessingException;
import it.cnr.istc.ale.api.Parameter;
import it.cnr.istc.ale.api.User;
import it.cnr.istc.ale.api.UserAPI;
import it.cnr.istc.ale.api.messages.LostStudent;
import it.cnr.istc.ale.api.messages.NewStudent;
import static it.cnr.istc.ale.server.Context.EMF;
import static it.cnr.istc.ale.server.Context.MAPPER;
import it.cnr.istc.ale.server.db.UserEntity;
import java.util.Collection;
import java.util.Collections;
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
import org.eclipse.paho.client.mqttv3.MqttException;

/**
 *
 * @author Riccardo De Benedictis
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
            EntityManager em = EMF.createEntityManager();
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
            UserEntity ue = EMF.createEntityManager().find(UserEntity.class, user_id);
            return new User(ue.getId(), ue.getFirstName(), ue.getLastName());
        } catch (NoResultException e) {
            throw new WebApplicationException(e.getLocalizedMessage(), Response.Status.NOT_FOUND);
        }
    }

    @Override
    @GET
    @Path("get_parameter_types")
    @Produces(MediaType.APPLICATION_JSON)
    public Map<String, Parameter> get_parameter_types(@QueryParam("student_id") long student_id) {
        Map<String, Parameter> types = Context.getContext().parameter_types.get(student_id);
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
        EntityManager em = EMF.createEntityManager();
        TypedQuery<UserEntity> query = em.createQuery("SELECT u FROM UserEntity u WHERE u.first_name LIKE :search_string OR u.last_name LIKE :search_string", UserEntity.class);
        query.setParameter("search_string", search_string);
        return query.getResultList().stream().map(usr -> new User(usr.getId(), usr.getFirstName(), usr.getLastName())).collect(Collectors.toList());
    }

    @Override
    @POST
    @Path("login")
    @Produces(MediaType.APPLICATION_JSON)
    public User login(@FormParam("email") String email, @FormParam("password") String password) {
        try {
            EntityManager em = EMF.createEntityManager();
            TypedQuery<UserEntity> query = em.createQuery("SELECT u FROM UserEntity u WHERE u.email = :email AND u.password = :password", UserEntity.class);
            query.setParameter("email", email);
            query.setParameter("password", password);
            UserEntity ue = query.getSingleResult();
            return new User(ue.getId(), ue.getFirstName(), ue.getLastName());
        } catch (NoResultException e) {
            throw new WebApplicationException(e.getLocalizedMessage(), Response.Status.UNAUTHORIZED);
        }
    }

    @Override
    @PUT
    @Path("add_teacher")
    public void add_teacher(@FormParam("student_id") long student_id, @FormParam("teacher_id") long teacher_id) {
        EntityManager em = EMF.createEntityManager();
        em.getTransaction().begin();
        UserEntity student = em.find(UserEntity.class, student_id);
        UserEntity teacher = em.find(UserEntity.class, teacher_id);
        student.addTeacher(teacher);
        teacher.addStudent(student);
        em.persist(student);
        em.getTransaction().commit();
        try {
            Context.getContext().mqtt.publish(teacher_id + "/input", MAPPER.writeValueAsBytes(new NewStudent(student_id)), 1, false);
        } catch (JsonProcessingException | MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    @Override
    @PUT
    @Path("remove_teacher")
    public void remove_teacher(@FormParam("student_id") long student_id, @FormParam("teacher_id") long teacher_id) {
        EntityManager em = EMF.createEntityManager();
        em.getTransaction().begin();
        UserEntity student = em.find(UserEntity.class, student_id);
        UserEntity teacher = em.find(UserEntity.class, teacher_id);
        student.removeTeacher(teacher);
        teacher.removeStudent(student);
        em.persist(student);
        em.getTransaction().commit();
        try {
            Context.getContext().mqtt.publish(teacher_id + "/input", MAPPER.writeValueAsBytes(new LostStudent(student_id)), 1, false);
        } catch (JsonProcessingException | MqttException ex) {
            LOG.log(Level.SEVERE, null, ex);
        }
    }

    @Override
    @GET
    @Path("get_teachers")
    @Produces(MediaType.APPLICATION_JSON)
    public Collection<User> get_teachers(@QueryParam("student_id") long student_id) {
        try {
            return EMF.createEntityManager().find(UserEntity.class, student_id).getTeachers().stream().map(st -> new User(st.getId(), st.getFirstName(), st.getLastName())).collect(Collectors.toList());
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
            return EMF.createEntityManager().find(UserEntity.class, teacher_id).getStudents().stream().map(st -> new User(st.getId(), st.getFirstName(), st.getLastName())).collect(Collectors.toList());
        } catch (NoResultException e) {
            throw new WebApplicationException(e.getLocalizedMessage(), Response.Status.NOT_FOUND);
        }
    }
}
