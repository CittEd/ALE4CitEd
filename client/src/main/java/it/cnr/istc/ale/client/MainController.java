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

import com.fasterxml.jackson.core.JsonProcessingException;
import it.cnr.istc.ale.api.Lesson;
import it.cnr.istc.ale.api.User;
import it.cnr.istc.ale.api.messages.Event;
import java.net.URL;
import java.util.ResourceBundle;
import java.util.logging.Level;
import java.util.logging.Logger;
import javafx.application.Platform;
import javafx.beans.binding.Bindings;
import javafx.beans.property.ObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Accordion;
import javafx.scene.control.Alert;
import javafx.scene.control.Alert.AlertType;
import javafx.scene.control.Button;
import javafx.scene.control.ListCell;
import javafx.scene.control.ListView;
import javafx.scene.control.MenuItem;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.control.cell.TextFieldTableCell;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
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
    private final StudentGrid student_grid = new StudentGrid();

    /**
     * Initializes the controller class.
     */
    @Override
    @SuppressWarnings("unchecked")
    public void initialize(URL url, ResourceBundle rb) {
        Stage stage = Context.getContext().getStage();

        ObjectProperty<User> user = Context.getContext().getUser();
        user.addListener((ObservableValue<? extends User> observable, User oldValue, User newValue) -> {
            if (newValue != null) {
                stage.setTitle("LECTurE (Learning Environment CiTtà Educante) - " + newValue.getFirstName());
            } else {
                stage.setTitle("LECTurE (Learning Environment CiTtà Educante)");
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
        lessons.setCellFactory((ListView<Lesson> param) -> new ListCell<Lesson>() {
            @Override
            protected void updateItem(Lesson lesson, boolean empty) {
                super.updateItem(lesson, empty);
                if (!empty) {
                    setText(lesson.getName());
                }
            }
        });
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

        HBox.setHgrow(student_grid, Priority.ALWAYS);

        students.getSelectionModel().selectedItemProperty().addListener((ObservableValue<? extends User> observable, User oldValue, User newValue) -> {
            if (newValue != null) {
                if (teach_h_box.getChildren().size() == 1) {
                    teach_h_box.getChildren().add(student_grid);
                } else if (teach_h_box.getChildren().get(1) != student_grid) {
                    teach_h_box.getChildren().set(1, student_grid);
                }
                student_grid.setUser(newValue);
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
        new LoginDialog().showAndWait().ifPresent(user -> {
            try {
                Context.getContext().login(user.getEmail(), user.getPassword());
            } catch (Exception e) {
                Alert alert = new Alert(AlertType.ERROR);
                alert.setTitle("Exception");
                alert.setHeaderText(e.getLocalizedMessage());
                alert.showAndWait();
            }
        });
    }

    public void logout() {
        Context.getContext().logout();
    }

    public void new_user() {
        new NewUserDialog().showAndWait().ifPresent(user -> {
            try {
                Context.getContext().new_user(user.getEmail(), user.getPassword(), user.getFirstName(), user.getLastName());
            } catch (Exception e) {
                Alert alert = new Alert(AlertType.ERROR);
                alert.setTitle("Exception");
                alert.setHeaderText(e.getLocalizedMessage());
                alert.showAndWait();
            }
        }
        );
    }

    public void exit() {
        Platform.exit();
    }

    public void add_teachers() {
        new AddTeachersDialog().showAndWait().ifPresent(teachers_to_add -> {
            for (User teacher : teachers_to_add) {
                Context.getContext().getUserResource().add_teacher(Context.getContext().getUser().get().getId(), teacher.getId());
                Context.getContext().add_teacher(teacher);
            }
        });
    }

    public void remove_selected_teachers() {
        for (User user : teachers.selectionModelProperty().get().getSelectedItems()) {
            Context.getContext().getUserResource().remove_teacher(Context.getContext().getUser().get().getId(), user.getId());
            Context.getContext().remove_teacher(user);
        }
    }

    public void add_lesson() {
        new AddLessonDialog().showAndWait().ifPresent(new_lesson -> {
            try {
                Lesson lesson = Context.getContext().getLessonResource().new_lesson(Context.getContext().getUser().get().getId(), new_lesson.getLessonName(), Context.MAPPER.writeValueAsString(new_lesson.getModel()), Context.MAPPER.writeValueAsString(new_lesson.getRoles()));
                Context.getContext().add_lesson(lesson);
            } catch (JsonProcessingException ex) {
                Alert alert = new Alert(AlertType.ERROR);
                alert.setTitle("Exception");
                alert.setHeaderText(ex.getLocalizedMessage());
                alert.showAndWait();
            }
        });
    }

    public void remove_selected_lessons() {
    }
}
