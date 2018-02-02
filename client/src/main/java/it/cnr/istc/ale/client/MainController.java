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

import it.cnr.istc.ale.api.Lesson;
import it.cnr.istc.ale.api.User;
import it.cnr.istc.ale.api.messages.Event;
import it.cnr.istc.ale.client.context.Context;
import it.cnr.istc.ale.client.context.UserContext.ParameterValue;
import java.net.URL;
import java.util.ResourceBundle;
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
import javafx.scene.control.TitledPane;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.control.cell.TextFieldTableCell;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.stage.Stage;

/**
 *
 * @author Riccardo De Benedictis
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
    private TableView<ParameterValue> par_values;
    @FXML
    private TableColumn<ParameterValue, String> par_names;
    @FXML
    private TableColumn<ParameterValue, String> par_vals;
    private final LessonGrid lesson_grid = new LessonGrid();
    private final StudentGrid student_grid = new StudentGrid();

    /**
     * Initializes the controller class.
     */
    @Override
    @SuppressWarnings("unchecked")
    public void initialize(URL url, ResourceBundle rb) {
        Stage stage = Context.getContext().getStage();

        ObjectProperty<User> user = Context.getContext().getUserContext().getUser();
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
        events.setItems(Context.getContext().getLearningContext().getEvents());
        following_lessons.setItems(Context.getContext().getLearningContext().getLessons());
        following_lessons.setCellFactory((ListView<Lesson> param) -> new ListCell<Lesson>() {
            @Override
            protected void updateItem(Lesson lesson, boolean empty) {
                super.updateItem(lesson, empty);
                if (empty) {
                    setText(null);
                } else {
                    setText(lesson.getName());
                }
            }
        });
        teachers.setItems(Context.getContext().getLearningContext().getTeachers());
        teachers.setCellFactory((ListView<User> param) -> new ListCell<User>() {
            @Override
            protected void updateItem(User user, boolean empty) {
                super.updateItem(user, empty);
                if (empty) {
                    setText(null);
                } else {
                    setText(user.getLastName() + " " + user.getFirstName());
                    if (Context.getContext().getConnectionContext().isOnline(user).get()) {
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
        lessons.setItems(Context.getContext().getTeachingContext().getLessons());
        lessons.setCellFactory((ListView<Lesson> param) -> new ListCell<Lesson>() {
            @Override
            protected void updateItem(Lesson lesson, boolean empty) {
                super.updateItem(lesson, empty);
                if (empty) {
                    setText(null);
                } else {
                    setText(lesson.getName());
                }
            }
        });
        add_lesson_button.disableProperty().bind(user.isNull());
        remove_lesson_button.disableProperty().bind(Bindings.isEmpty(lessons.selectionModelProperty().get().getSelectedItems()));

        HBox.setHgrow(lesson_grid, Priority.ALWAYS);

        lessons.getSelectionModel().selectedItemProperty().addListener((ObservableValue<? extends Lesson> observable, Lesson oldValue, Lesson newValue) -> {
            if (newValue != null) {
                if (teach_h_box.getChildren().size() == 1) {
                    teach_h_box.getChildren().add(lesson_grid);
                } else if (teach_h_box.getChildren().get(1) != lesson_grid) {
                    teach_h_box.getChildren().set(1, lesson_grid);
                }
                lesson_grid.setLesson(newValue);
            }
        });

        students.setItems(Context.getContext().getTeachingContext().getStudents());
        students.setCellFactory((ListView<User> param) -> new ListCell<User>() {
            @Override
            protected void updateItem(User user, boolean empty) {
                super.updateItem(user, empty);
                if (empty) {
                    setText(null);
                } else {
                    setText(user.getLastName() + " " + user.getFirstName());
                    if (Context.getContext().getConnectionContext().isOnline(user).get()) {
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

        teach_accord.expandedPaneProperty().addListener((ObservableValue<? extends TitledPane> observable, TitledPane oldValue, TitledPane newValue) -> {
            if (newValue != null) {
                switch (teach_accord.getPanes().indexOf(newValue)) {
                    case 0:
                        if (!lessons.getSelectionModel().isEmpty()) {
                            if (teach_h_box.getChildren().size() == 1) {
                                teach_h_box.getChildren().add(lesson_grid);
                            } else if (teach_h_box.getChildren().get(1) != lesson_grid) {
                                teach_h_box.getChildren().set(1, lesson_grid);
                            }
                        }
                        break;
                    case 1:
                        if (!students.getSelectionModel().isEmpty()) {
                            if (teach_h_box.getChildren().size() == 1) {
                                teach_h_box.getChildren().add(student_grid);
                            } else if (teach_h_box.getChildren().get(1) != student_grid) {
                                teach_h_box.getChildren().set(1, student_grid);
                            }
                        }
                        break;
                }
            }
        });

        par_values.setItems(Context.getContext().getUserContext().getParameterValues());
        par_values.setColumnResizePolicy(TableView.CONSTRAINED_RESIZE_POLICY);
        par_names.setCellValueFactory(new PropertyValueFactory("name"));
        par_vals.setCellValueFactory(new PropertyValueFactory("value"));
        par_vals.setCellFactory(TextFieldTableCell.forTableColumn());
        par_vals.setOnEditCommit((TableColumn.CellEditEvent<ParameterValue, String> event) -> {
            ParameterValue par_value = (ParameterValue) event.getTableView().getItems().get(event.getTablePosition().getRow());
            String[] par_name = par_value.nameProperty().get().split("\\.");
            Context.getContext().getUserContext().setParameterValue(par_name[0], par_name[1], event.getNewValue());
        });
    }

    public void login() {
        new LoginDialog().showAndWait().ifPresent(user -> {
            try {
                Context.getContext().getConnectionContext().login(user.getEmail(), user.getPassword());
            } catch (Exception e) {
                Alert alert = new Alert(AlertType.ERROR);
                alert.setTitle("Exception");
                alert.setHeaderText(e.getLocalizedMessage());
                alert.showAndWait();
            }
        });
    }

    public void logout() {
        Context.getContext().getConnectionContext().logout();
    }

    public void new_user() {
        new NewUserDialog().showAndWait().ifPresent(user -> {
            try {
                Context.getContext().getConnectionContext().newUser(user.getEmail(), user.getPassword(), user.getFirstName(), user.getLastName());
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
                Context.getContext().addTeacher(teacher);
            }
        });
    }

    public void remove_selected_teachers() {
        for (User user : teachers.selectionModelProperty().get().getSelectedItems()) {
            Context.getContext().removeTeacher(user);
        }
    }

    public void add_lesson() {
        new AddLessonDialog().showAndWait().ifPresent(new_lesson -> {
            Context.getContext().newLesson(new_lesson.getLessonName(), new_lesson.getModel(), new_lesson.getRoles());
        });
    }

    public void remove_selected_lessons() {
        for (Lesson lesson : lessons.selectionModelProperty().get().getSelectedItems()) {
            Context.getContext().removeLesson(lesson);
        }
    }
}
