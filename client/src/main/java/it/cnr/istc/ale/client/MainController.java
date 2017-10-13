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
import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;
import java.util.logging.Level;
import java.util.logging.Logger;
import javafx.application.Platform;
import javafx.beans.property.ObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.ListView;
import javafx.scene.control.MenuItem;
import javafx.stage.Modality;
import javafx.stage.Stage;

/**
 * FXML Controller class
 *
 * @author Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
 */
public class MainController implements Initializable {

    @FXML
    private MenuItem login;
    @FXML
    private MenuItem logout;
    @FXML
    private MenuItem new_user;
    @FXML
    private ListView<User> teachers;
    @FXML
    private ListView<User> students;

    /**
     * Initializes the controller class.
     */
    @Override
    public void initialize(URL url, ResourceBundle rb) {
        ObjectProperty<User> user = Context.getContext().getUser();
        login.disableProperty().bind(user.isNotNull());
        new_user.disableProperty().bind(user.isNotNull());
        logout.disableProperty().bind(user.isNull());
        user.addListener((ObservableValue<? extends User> observable, User oldValue, User newValue) -> {
            if (newValue != null) {
                Context.getContext().getStage().setTitle("Active Learning Environment - " + newValue.getFirstName());
            } else {
                Context.getContext().getStage().setTitle("Active Learning Environment");
            }
        });
        teachers.setItems(Context.getContext().getTeachers());
        students.setItems(Context.getContext().getStudents());
    }

    public void login() {
        try {
            Stage tmp_stage = Context.getContext().getStage();

            Stage login_stage = new Stage();
            Parent root = FXMLLoader.load(getClass().getResource("/fxml/Login.fxml"));

            Scene scene = new Scene(root);

            login_stage.setTitle("Login");
            login_stage.setScene(scene);
            login_stage.initOwner(tmp_stage);
            login_stage.initModality(Modality.APPLICATION_MODAL);
            login_stage.setResizable(false);
            Context.getContext().setStage(login_stage);
            login_stage.showAndWait();

            Context.getContext().setStage(tmp_stage);
        } catch (IOException ex) {
            Logger.getLogger(MainController.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    public void logout() {
        Context.getContext().logout();
    }

    public void new_user() {
        try {
            Stage tmp_stage = Context.getContext().getStage();

            Stage login_stage = new Stage();
            Parent root = FXMLLoader.load(getClass().getResource("/fxml/NewUser.fxml"));

            Scene scene = new Scene(root);

            login_stage.setTitle("New user");
            login_stage.setScene(scene);
            login_stage.initOwner(tmp_stage);
            login_stage.initModality(Modality.APPLICATION_MODAL);
            login_stage.setResizable(false);
            Context.getContext().setStage(login_stage);
            login_stage.showAndWait();

            Context.getContext().setStage(tmp_stage);
        } catch (IOException ex) {
            Logger.getLogger(MainController.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    public void exit() {
        Platform.exit();
    }
}
