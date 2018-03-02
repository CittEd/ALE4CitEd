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
package it.cnr.istc.lecture.webapp;

import it.cnr.istc.lecture.webapp.api.Credentials;
import it.cnr.istc.lecture.webapp.api.NewUserRequest;
import it.cnr.istc.lecture.webapp.api.Parameter;
import it.cnr.istc.lecture.webapp.api.User;
import it.cnr.istc.lecture.webapp.entities.UserEntity;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import javax.annotation.Resource;
import javax.ejb.EJB;
import javax.persistence.EntityManager;
import javax.persistence.NoResultException;
import javax.persistence.PersistenceContext;
import javax.persistence.TypedQuery;
import javax.transaction.HeuristicMixedException;
import javax.transaction.HeuristicRollbackException;
import javax.transaction.NotSupportedException;
import javax.transaction.RollbackException;
import javax.transaction.SystemException;
import javax.transaction.UserTransaction;
import javax.ws.rs.Consumes;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.UriInfo;
import javax.ws.rs.Produces;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

/**
 * REST Web Service
 *
 * @author Riccardo De Benedictis
 */
@Path("/")
public class LECTurEResource {

    private static final Logger LOG = Logger.getLogger(LECTurEResource.class.getName());
    @Context
    private UriInfo context;
    @PersistenceContext
    private EntityManager em;
    @Resource
    private UserTransaction utx;
    @EJB
    private LECTurEBean ctx;

    /**
     * Creates a new instance of UsersResource
     */
    public LECTurEResource() {
    }

    @POST
    @Path("newUser")
    @Produces(MediaType.APPLICATION_JSON)
    @Consumes(MediaType.APPLICATION_JSON)
    public User newUser(NewUserRequest new_user) {
        try {
            utx.begin();
            UserEntity ue = new UserEntity();
            ue.setEmail(new_user.getEmail());
            ue.setPassword(new_user.getPassword());
            ue.setFirstName(new_user.getFirstName());
            ue.setLastName(new_user.getLastName());
            em.persist(ue);
            utx.commit();
            return new User(ue.getId(), ue.getFirstName(), ue.getLastName());
        } catch (IllegalStateException | SecurityException | HeuristicMixedException | HeuristicRollbackException | NotSupportedException | RollbackException | SystemException ex) {
            try {
                utx.rollback();
            } catch (IllegalStateException | SecurityException | SystemException ex1) {
                LOG.log(Level.SEVERE, null, ex1);
            }
            throw new WebApplicationException(ex.getMessage());
        }
    }

    @GET
    @Path("users")
    @Produces(MediaType.APPLICATION_JSON)
    public Collection<User> getUsers() {
        List<UserEntity> users = em.createQuery("SELECT u FROM UserEntity u", UserEntity.class).getResultList();
        return users.stream().map(u -> new User(u.getId(), u.getFirstName(), u.getLastName())).collect(Collectors.toList());
    }

    @GET
    @Path("find_users/{search_string}")
    @Produces(MediaType.APPLICATION_JSON)
    public Collection<User> findUsers(@PathParam("search_string") String search_string) {
        TypedQuery<UserEntity> query = em.createQuery("SELECT u FROM UserEntity u WHERE u.firstName LIKE :search_string OR u.lastName LIKE :search_string", UserEntity.class);
        query.setParameter("search_string", search_string);
        return query.getResultList().stream().map(usr -> new User(usr.getId(), usr.getFirstName(), usr.getLastName())).collect(Collectors.toList());
    }

    @GET
    @Path("users/{user_id}")
    @Produces(MediaType.APPLICATION_JSON)
    public User getUser(@PathParam("user_id") long user_id) {
        UserEntity u = em.find(UserEntity.class, user_id);
        if (u != null) {
            return new User(u.getId(), u.getFirstName(), u.getLastName());
        } else {
            throw new WebApplicationException("Cannot find user id");
        }
    }

    @POST
    @Path("login")
    @Produces(MediaType.APPLICATION_JSON)
    @Consumes(MediaType.APPLICATION_JSON)
    public User login(Credentials credentials) {
        try {
            TypedQuery<UserEntity> query = em.createQuery("SELECT u FROM UserEntity u WHERE u.email = :email AND u.password = :password", UserEntity.class);
            query.setParameter("email", credentials.getEmail());
            query.setParameter("password", credentials.getPassword());
            UserEntity ue = query.getSingleResult();
            return new User(ue.getId(), ue.getFirstName(), ue.getLastName());
        } catch (NoResultException e) {
            throw new WebApplicationException(e.getLocalizedMessage(), Response.Status.UNAUTHORIZED);
        }
    }

    @GET
    @Path("parameter_types/{user_id}")
    @Produces(MediaType.APPLICATION_JSON)
    public Map<String, Parameter> getParameterTypes(@PathParam("user_id") long user_id) {
        Map<String, Parameter> par_types = ctx.getParameters(user_id);
        if (par_types != null) {
            return par_types;
        } else {
            throw new WebApplicationException("Cannot find user id");
        }
    }
}
