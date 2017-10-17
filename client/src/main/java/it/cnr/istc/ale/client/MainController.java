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

import it.cnr.istc.ale.api.Lesson;
import it.cnr.istc.ale.api.User;
import it.cnr.istc.ale.api.messages.Event;
import java.io.IOException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.ResourceBundle;
import java.util.logging.Level;
import java.util.logging.Logger;
import javafx.application.Platform;
import javafx.beans.binding.Bindings;
import javafx.beans.property.ObjectProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Accordion;
import javafx.scene.control.Button;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.control.MenuItem;
import javafx.scene.control.MultipleSelectionModel;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.control.cell.TextFieldTableCell;
import javafx.scene.layout.HBox;
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
    private Accordion learn_accord;
    @FXML
    private ListView<Event> events;
    @FXML
    private ListView<Lesson> following_lessons;
    @FXML
    private ListView<User> teachers;
    @FXML
    private Button add_teachers_button;
    @FXML
    private Button remove_teachers_button;
    @FXML
    private HBox teach_h_box;
    @FXML
    private Accordion teach_accord;
    @FXML
    private ListView<Lesson> lessons;
    @FXML
    private Button add_lesson_button;
    @FXML
    private Button remove_lesson_button;
    @FXML
    private ListView<User> students;
    @FXML
    private TableView<Context.ParameterValue> par_values;
    @FXML
    private TableColumn<Context.ParameterValue, String> par_names;
    @FXML
    private TableColumn<Context.ParameterValue, String> par_vals;
    private final Map<User, Parent> student_parent = new HashMap<>();

    /**
     * Initializes the controller class.
     */
    @Override
    public void initialize(URL url, ResourceBundle rb) {
        Stage stage = Context.getContext().getStage();

        ObjectProperty<User> user = Context.getContext().getUser();
        user.addListener((ObservableValue<? extends User> observable, User oldValue, User newValue) -> {
            if (newValue != null) {
                stage.setTitle("Active Learning Environment - " + newValue.getFirstName());
            } else {
                stage.setTitle("Active Learning Environment");
            }
        });

        login.disableProperty().bind(user.isNotNull());
        new_user.disableProperty().bind(user.isNotNull());
        logout.disableProperty().bind(user.isNull());

        learn_accord.setExpandedPane(learn_accord.getPanes().get(0));
        events.setItems(Context.getContext().getEvents());
        following_lessons.setItems(Context.getContext().getFollowingLessons());
        teachers.setItems(Context.getContext().getTeachers());
        teachers.setCellFactory((ListView<User> param) -> new ListCell<User>() {
            @Override
            protected void updateItem(User user, boolean empty) {
                super.updateItem(user, empty);
                if (!empty) {
                    setText(user.getLastName() + " " + user.getFirstName());
                    if (Context.getContext().is_online(user).get()) {
                        setStyle("-fx-text-fill: black;");
                    } else {
                        setStyle("-fx-text-fill: gray;");
                    }
                }
            }
        });
        add_teachers_button.disableProperty().bind(user.isNull());
        remove_teachers_button.disableProperty().bind(Bindings.isEmpty(teachers.selectionModelProperty().get().getSelectedItems()));

        teach_accord.setExpandedPane(teach_accord.getPanes().get(0));
        lessons.setItems(Context.getContext().getLessons());
        add_lesson_button.disableProperty().bind(user.isNull());
        remove_lesson_button.disableProperty().bind(Bindings.isEmpty(lessons.selectionModelProperty().get().getSelectedItems()));
        students.setItems(Context.getContext().getStudents());
        students.setCellFactory((ListView<User> param) -> new ListCell<User>() {
            @Override
            protected void updateItem(User user, boolean empty) {
                super.updateItem(user, empty);
                if (!empty) {
                    setText(user.getLastName() + " " + user.getFirstName());
                    if (Context.getContext().is_online(user).get()) {
                        setStyle("-fx-text-fill: black;");
                    } else {
                        setStyle("-fx-text-fill: gray;");
                    }
                }
            }
        });
        students.selectionModelProperty().addListener(new ChangeListener<MultipleSelectionModel<User>>() {
            @Override
            public void changed(ObservableValue<? extends MultipleSelectionModel<User>> observable, MultipleSelectionModel<User> oldValue, MultipleSelectionModel<User> newValue) {
                User u = newValue.getSelectedItems().get(0);
                if (!student_parent.containsKey(u)) {
                    FXMLLoader loader = new FXMLLoader(getClass().getResource("fxml/Student.fxml"));
                    try {
                        Parent root = (Parent) loader.load();
                        StudentController sc = loader.getController();
                        sc.setUser(u);
                        student_parent.put(u, root);
                    } catch (IOException ex) {
                        Logger.getLogger(MainController.class.getName()).log(Level.SEVERE, null, ex);
                    }
                }
                teach_h_box.getChildren().set(1, student_parent.get(u));
            }
        });

        par_values.setItems(Context.getContext().getParameterValues());
        par_values.setColumnResizePolicy(TableView.CONSTRAINED_RESIZE_POLICY);
        par_names.setCellValueFactory(new PropertyValueFactory("name"));
        par_vals.setCellValueFactory(new PropertyValueFactory("value"));
        par_vals.setCellFactory(TextFieldTableCell.forTableColumn());
        par_vals.setOnEditCommit((TableColumn.CellEditEvent<Context.ParameterValue, String> event) -> {
            Context.ParameterValue par_val = (Context.ParameterValue) event.getTableView().getItems().get(event.getTablePosition().getRow());
            par_val.valueProperty().set(event.getNewValue());
            Context.getContext().par_update(par_val.nameProperty().get().split("\\.")[0]);
        });
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

    public void add_teachers() {
        try {
            Stage tmp_stage = Context.getContext().getStage();

            Stage login_stage = new Stage();
            Parent root = FXMLLoader.load(getClass().getResource("/fxml/AddTeachers.fxml"));

            Scene scene = new Scene(root);

            login_stage.setTitle("Add teachers");
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

    public void remove_selected_teachers() {
        for (User user : teachers.selectionModelProperty().get().getSelectedItems()) {
            Context.getContext().getUserResource().remove_teacher(Context.getContext().getUser().get().getId(), user.getId());
            Context.getContext().remove_teacher(user);
        }
    }

    public void add_lesson() {
    }

    public void remove_selected_lessons() {
    }
}
