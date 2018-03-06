/*
 * Copyright (C) 2018 ISTC - CNR
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
package it.cnr.istc.lecture.desktopapp;

import it.cnr.istc.lecture.api.User;
import java.net.URL;
import java.util.ResourceBundle;
import javafx.beans.property.ObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Alert;
import javafx.scene.control.Alert.AlertType;
import javafx.scene.control.MenuItem;
import javafx.scene.control.TabPane;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;

/**
 *
 * @author Riccardo De Benedictis
 */
public class MainController implements Initializable {

    @FXML
    private VBox root;
    @FXML
    private MenuItem login;
    @FXML
    private MenuItem logout;
    @FXML
    private MenuItem new_user;
    @FXML
    private TabPane tab_pane;

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        Stage stage = Context.getContext().getStage();
        stage.setTitle("LECTurE (Learning Environment CiTtà Educante)");

        ObjectProperty<User> user = Context.getContext().userProperty();
        user.addListener((ObservableValue<? extends User> observable, User oldValue, User newValue) -> {
            if (newValue != null) {
                stage.setTitle("LECTurE (Learning Environment CiTtà Educante) - " + newValue.getFirstName());
            } else {
                stage.setTitle("LECTurE (Learning Environment CiTtà Educante)");
            }
        });
    }

    @FXML
    private void login(ActionEvent event) {
        LoginDialog login_dialog = new LoginDialog();
        login_dialog.getDialogPane().getStylesheets().addAll(Context.getContext().getStage().getScene().getStylesheets());
        login_dialog.showAndWait().ifPresent(user -> {
            try {
                Context.getContext().login(user.getEmail(), user.getPassword());
            } catch (Exception e) {
                Alert alert = new Alert(AlertType.ERROR);
                alert.getDialogPane().getStylesheets().addAll(Context.getContext().getStage().getScene().getStylesheets());
                alert.setTitle("Exception");
                alert.setHeaderText(e.getLocalizedMessage());
                alert.showAndWait();
            }
        });
    }

    @FXML
    private void logout(ActionEvent event) {
    }

    @FXML
    private void new_user(ActionEvent event) {
        NewUserDialog new_user_dialog = new NewUserDialog();
        new_user_dialog.getDialogPane().getStylesheets().addAll(Context.getContext().getStage().getScene().getStylesheets());
        new_user_dialog.showAndWait().ifPresent(user -> {
            try {
                Context.getContext().newUser(user.getEmail(), user.getPassword(), user.getFirstName(), user.getPassword());
            } catch (Exception e) {
                Alert alert = new Alert(AlertType.ERROR);
                alert.getDialogPane().getStylesheets().addAll(Context.getContext().getStage().getScene().getStylesheets());
                alert.setTitle("Exception");
                alert.setHeaderText(e.getLocalizedMessage());
                alert.showAndWait();
            }
        }
        );
    }

    @FXML
    private void exit(ActionEvent event) {
    }
}
