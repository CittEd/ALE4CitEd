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
import java.net.URL;
import java.util.ResourceBundle;
import javafx.beans.binding.Bindings;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.control.TextField;

/**
 * FXML Controller class
 *
 * @author Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
 */
public class AddTeachersController implements Initializable {

    @FXML
    private TextField search_text;
    @FXML
    private Button find_button;
    private final ObservableList<User> find_users = FXCollections.observableArrayList();
    @FXML
    private ListView<User> users;
    @FXML
    private Button add_users;

    /**
     * Initializes the controller class.
     */
    @Override
    public void initialize(URL url, ResourceBundle rb) {
        find_button.disableProperty().bind(search_text.textProperty().isEmpty());

        find_users.sort((User o1, User o2) -> {
            int res = o1.getLastName().compareToIgnoreCase(o2.getLastName());
            if (res != 0) {
                return res;
            }
            return o1.getFirstName().compareToIgnoreCase(o2.getFirstName());
        });

        users.setItems(find_users);
        users.setCellFactory((ListView<User> param) -> new ListCell<User>() {
            @Override
            protected void updateItem(User user, boolean empty) {
                super.updateItem(user, empty);
                if (!empty) {
                    setText(user.getLastName() + " " + user.getFirstName());
                    if (Context.getContext().getUserResource().is_online(user.getId())) {
                        setStyle("-fx-text-fill: black;");
                    } else {
                        setStyle("-fx-text-fill: gray;");
                    }
                }
            }
        });

        add_users.disableProperty().bind(Bindings.isEmpty(users.selectionModelProperty().get().getSelectedItems()));
    }

    public void find() {
        find_users.addAll(Context.getContext().getUserResource().find_users(search_text.getText()));
    }

    public void add_users() {
        for (User user : users.selectionModelProperty().get().getSelectedItems()) {
            Context.getContext().add_teacher(user);
        }
        Context.getContext().getStage().close();
    }
}
