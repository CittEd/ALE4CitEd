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
package it.cnr.istc.ale.client;

import javafx.scene.control.ButtonBar.ButtonData;
import javafx.scene.control.ButtonType;
import javafx.scene.control.Dialog;
import javafx.scene.control.Label;
import javafx.scene.control.PasswordField;
import javafx.scene.control.TextField;
import javafx.scene.layout.GridPane;

/**
 *
 * @author Riccardo De Benedictis
 */
public class NewUserDialog extends Dialog<NewUserDialog.NewUserResult> {

    private final GridPane grid = new GridPane();
    private final TextField email_field = new TextField();
    private final PasswordField password_field = new PasswordField();
    private final TextField first_name_field = new TextField();
    private final TextField last_name_field = new TextField();
    private final ButtonType create_button = new ButtonType("Create", ButtonData.OK_DONE);

    public NewUserDialog() {
        setTitle("New user");

        grid.setHgap(10);
        grid.setVgap(10);
        email_field.setPromptText("E-Mail");
        grid.add(new Label("E-Mail:"), 0, 0);
        grid.add(email_field, 1, 0);
        password_field.setPromptText("Password");
        grid.add(new Label("Password:"), 0, 1);
        grid.add(password_field, 1, 1);
        first_name_field.setPromptText("First name");
        grid.add(new Label("First name:"), 0, 2);
        grid.add(first_name_field, 1, 2);
        grid.add(new Label("Last name:"), 0, 3);
        last_name_field.setPromptText("Last name");
        grid.add(last_name_field, 1, 3);
        getDialogPane().setContent(grid);

        getDialogPane().getButtonTypes().add(create_button);
        getDialogPane().lookupButton(create_button).disableProperty().bind(email_field.textProperty().isEmpty().or(password_field.textProperty().isEmpty()).or(first_name_field.textProperty().isEmpty()).or(last_name_field.textProperty().isEmpty()));
        setResultConverter((ButtonType param) -> param == create_button ? new NewUserResult(email_field.getText(), password_field.getText(), first_name_field.getText(), last_name_field.getText()) : null);
    }

    public static class NewUserResult {

        private final String email;
        private final String password;
        private final String first_name;
        private final String last_name;

        private NewUserResult(String email, String password, String first_name, String last_name) {
            this.email = email;
            this.password = password;
            this.first_name = first_name;
            this.last_name = last_name;
        }

        public String getEmail() {
            return email;
        }

        public String getPassword() {
            return password;
        }

        public String getFirstName() {
            return first_name;
        }

        public String getLastName() {
            return last_name;
        }
    }
}
