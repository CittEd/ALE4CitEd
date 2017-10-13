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
import it.cnr.istc.ale.server.App;
import it.cnr.istc.ale.server.db.UserEntity;
import java.util.Collection;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.persistence.EntityManager;
import javax.persistence.NoResultException;
import javax.persistence.RollbackException;
import javax.persistence.TypedQuery;
import javax.ws.rs.FormParam;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
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
            throw new WebApplicationException(Response.Status.FORBIDDEN);
        }
    }

    @Override
    public User get_user(long id) {
        LOG.log(Level.INFO, "get user: {0}", id);
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
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
            throw new WebApplicationException(Response.Status.UNAUTHORIZED);
        }
    }

    @Override
    public Collection<User> get_followed_users(long id) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public Collection<User> get_followed_by_users(long id) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
}
