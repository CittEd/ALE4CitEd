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
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Region;
import javafx.stage.Stage;

/**
 *
 * @author Riccardo De Benedictis
 */
public class AddTeachersDialog extends Dialog<User[]> {

    private final GridPane grid = new GridPane();
    private final TextField find_field = new TextField();
    private final Button find_button = new Button(Context.LANGUAGE.getString("FIND"));
    private final ObservableList<User> found_users = FXCollections.observableArrayList();
    private ListView<User> found_users_list_view = new ListView<>(found_users);
    private final ButtonType add_button = new ButtonType(Context.LANGUAGE.getString("ADD"), ButtonBar.ButtonData.OK_DONE);

    public AddTeachersDialog() {
        setTitle(Context.LANGUAGE.getString("ADD TEACHERS"));

        grid.setHgap(10);
        grid.setVgap(10);
        grid.add(new Label(Context.LANGUAGE.getString("FIND") + ":"), 0, 0);
        find_field.setPromptText(Context.LANGUAGE.getString("TEACHER"));
        grid.add(find_field, 1, 0);
        grid.add(find_button, 2, 0);
        grid.add(found_users_list_view, 0, 1, 3, 1);
        getDialogPane().setContent(grid);

        found_users_list_view.setCellFactory((ListView<User> param) -> new ListCell<User>() {
            @Override
            protected void updateItem(User user, boolean empty) {
                super.updateItem(user, empty);
                if (empty) {
                    setText(null);
                } else {
                    setText(user.first_name + " " + user.last_name);
                }
            }
        });
        found_users_list_view.setOnMouseClicked((MouseEvent event) -> {
            if (event.getClickCount() == 2 && !found_users_list_view.getSelectionModel().isEmpty()) {
                ((Button) getDialogPane().lookupButton(add_button)).fire();
            }
        });

        find_button.disableProperty().bind(find_field.textProperty().isEmpty());
        find_button.setOnAction((ActionEvent event) -> {
            found_users.addAll(Context.getContext().findUsers(find_field.getText()));
        });

        getDialogPane().getButtonTypes().add(add_button);
        getDialogPane().lookupButton(add_button).disableProperty().bind(Bindings.isEmpty(found_users_list_view.selectionModelProperty().get().getSelectedItems()));
        getDialogPane().setMinHeight(Region.USE_PREF_SIZE);
        ((Stage) getDialogPane().getScene().getWindow()).getIcons().addAll(Context.getContext().getStage().getIcons());
        setResultConverter((ButtonType param) -> param == add_button ? found_users_list_view.getSelectionModel().getSelectedItems().toArray(new User[found_users_list_view.getSelectionModel().getSelectedItems().size()]) : null);
    }
}
