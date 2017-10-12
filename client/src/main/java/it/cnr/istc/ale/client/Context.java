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
import java.util.Collection;
import javafx.scene.control.Alert;
import javafx.stage.Stage;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.client.Client;
import javax.ws.rs.client.ClientBuilder;
import javax.ws.rs.client.Entity;
import javax.ws.rs.core.Form;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

/**
 *
 * @author Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
 */
public class Context implements UserAPI {

    private static final String HOST = "localhost";
    private static final String SERVICE_PORT = "8080";
    private static final String REST_URI = "http://" + HOST + ":" + SERVICE_PORT;
    private static final String MQTT_PORT = "1883";
    private static Context context;

    public static Context getContext() {
        if (context == null) {
            context = new Context();
        }
        return context;
    }
    private Client client = ClientBuilder.newClient();
    private Stage stage;
    private User user;

    private Context() {
    }

    public Stage getStage() {
        return stage;
    }

    public void setStage(Stage stage) {
        this.stage = stage;
    }

    public User getUser() {
        return user;
    }

    @Override
    public User new_user(String email, String password, String first_name, String last_name) {
        try {
            Form form = new Form();
            form.param("email", email);
            form.param("password", password);
            form.param("first_name", first_name);
            form.param("last_name", last_name);
            this.user = client.target(REST_URI)
                    .path("users")
                    .path("new_user")
                    .request(MediaType.APPLICATION_JSON)
                    .post(Entity.form(form), User.class);
        } catch (WebApplicationException e) {
            Alert alert = new Alert(Alert.AlertType.ERROR);
            alert.setTitle("User creation error");
            alert.setHeaderText(e.getLocalizedMessage());
            alert.showAndWait();
        }
        return user;
    }

    @Override
    public User get_user(long id) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public User login(String email, String password) {
        try {
            Form form = new Form();
            form.param("email", email);
            form.param("password", password);
            this.user = client.target(REST_URI)
                    .path("users")
                    .path("login")
                    .request(MediaType.APPLICATION_JSON)
                    .post(Entity.form(form), User.class);
        } catch (WebApplicationException e) {
            Alert alert = new Alert(Alert.AlertType.ERROR);
            alert.setTitle("Login error");
            alert.setHeaderText(e.getLocalizedMessage());
            alert.showAndWait();
        }
        return user;
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
