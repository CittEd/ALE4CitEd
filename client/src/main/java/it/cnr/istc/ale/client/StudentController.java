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
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.control.cell.PropertyValueFactory;

/**
 * FXML Controller class
 *
 * @author Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
 */
public class StudentController implements Initializable {

    @FXML
    private TextField first_name;
    @FXML
    private TextField last_name;
    @FXML
    private TableView<Context.ParameterValue> par_values;
    @FXML
    private TableColumn<Context.ParameterValue, String> par_names;
    @FXML
    private TableColumn<Context.ParameterValue, String> par_vals;

    /**
     * Initializes the controller class.
     */
    @Override
    public void initialize(URL url, ResourceBundle rb) {
        par_values.setColumnResizePolicy(TableView.CONSTRAINED_RESIZE_POLICY);
        par_names.setCellValueFactory(new PropertyValueFactory("name"));
        par_vals.setCellValueFactory(new PropertyValueFactory("value"));
    }

    public void setUser(User user) {
        first_name.setText(user.getFirstName());
        last_name.setText(user.getLastName());
        par_values.setItems(Context.getContext().getParameterValues(user.getId()));
    }
}
