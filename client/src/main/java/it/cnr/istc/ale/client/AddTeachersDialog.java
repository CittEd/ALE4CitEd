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
import javafx.beans.binding.Bindings;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.ButtonType;
import javafx.scene.control.Dialog;
import javafx.scene.control.Label;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.control.TextField;
import javafx.scene.layout.GridPane;

/**
 *
 * @author Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
 */
public class AddTeachersDialog extends Dialog<User[]> {

    private final ButtonType add_button = new ButtonType("Add", ButtonBar.ButtonData.OK_DONE);
    private final GridPane grid = new GridPane();
    private final TextField find_field = new TextField();
    private final Button find_button = new Button("Find");
    private final ObservableList<User> found_users = FXCollections.observableArrayList();
    private ListView<User> found_users_view = new ListView<>(found_users);

    public AddTeachersDialog() {
        setTitle("Add teachers");

        grid.setHgap(10);
        grid.setVgap(10);
        grid.add(new Label("Find:"), 0, 0);
        find_field.setPromptText("Teacher");
        grid.add(find_field, 1, 0);
        grid.add(find_button, 2, 0);
        grid.add(found_users_view, 0, 1, 3, 1);
        getDialogPane().setContent(grid);

        found_users_view.setCellFactory((ListView<User> param) -> new ListCell<User>() {
            @Override
            protected void updateItem(User user, boolean empty) {
                super.updateItem(user, empty);
                if (!empty) {
                    setText(user.getLastName() + " " + user.getFirstName());
                }
            }
        });

        find_button.disableProperty().bind(find_field.textProperty().isEmpty());
        find_button.setOnAction((ActionEvent event) -> {
            found_users.addAll(Context.getContext().getUserResource().find_users(find_field.getText()));
        });

        getDialogPane().getButtonTypes().add(add_button);
        getDialogPane().lookupButton(add_button).disableProperty().bind(Bindings.isEmpty(found_users_view.selectionModelProperty().get().getSelectedItems()));
        setResultConverter((ButtonType param) -> param == add_button ? found_users_view.getSelectionModel().getSelectedItems().toArray(new User[found_users_view.getSelectionModel().getSelectedItems().size()]) : null);
    }
}
