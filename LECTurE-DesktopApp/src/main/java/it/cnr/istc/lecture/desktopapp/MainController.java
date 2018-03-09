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
import it.cnr.istc.lecture.api.messages.Event;
import it.cnr.istc.lecture.api.messages.QuestionEvent;
import it.cnr.istc.lecture.api.messages.TextEvent;
import it.cnr.istc.lecture.api.messages.URLEvent;
import it.cnr.istc.lecture.desktopapp.Context.ParameterValue;
import java.net.URL;
import java.util.ResourceBundle;
import javafx.application.Platform;
import javafx.beans.binding.Bindings;
import javafx.beans.property.ObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.event.ActionEvent;
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
import javafx.stage.Stage;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.Glyph;

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
    private ListView<FollowingLessonContext> following_lessons;
    @FXML
    private ListView<TeacherContext> teachers;
    @FXML
    private Button add_teachers_button;
    @FXML
    private Button remove_teachers_button;
    @FXML
    private Accordion teach_accord;
    @FXML
    private ListView<TeachingLessonContext> teaching_lessons;
    @FXML
    private Button add_lesson_button;
    @FXML
    private Button remove_lessons_button;
    @FXML
    private ListView<StudentContext> students;
    @FXML
    private TableView<ParameterValue> parameters;
    @FXML
    private TableColumn<ParameterValue, String> par_names;
    @FXML
    private TableColumn<ParameterValue, String> par_vals;

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        Stage stage = Context.getContext().getStage();
        stage.setTitle("LECTurE (Learning Environment CiTtà Educante)");

        ObjectProperty<User> user = Context.getContext().userProperty();
        user.addListener((ObservableValue<? extends User> observable, User oldValue, User newValue) -> {
            if (newValue != null) {
                stage.setTitle("LECTurE (Learning Environment CiTtà Educante) - " + newValue.first_name);
            } else {
                stage.setTitle("LECTurE (Learning Environment CiTtà Educante)");
            }
        });

        login.disableProperty().bind(user.isNotNull());
        new_user.disableProperty().bind(user.isNotNull());
        logout.disableProperty().bind(user.isNull());

        learn_accord.setExpandedPane(learn_accord.getPanes().get(0));

        events.setItems(Context.getContext().eventsProperty());
        events.setCellFactory((ListView<Event> param) -> new ListCell<Event>() {
            @Override
            protected void updateItem(Event event, boolean empty) {
                super.updateItem(event, empty);
                if (empty) {
                    setText(null);
                } else {
                    if (event instanceof TextEvent) {
                        setText(((TextEvent) event).content);
                    } else if (event instanceof URLEvent) {
                        setText(((URLEvent) event).content);
                    } else if (event instanceof QuestionEvent) {
                        setText(((QuestionEvent) event).question);
                    }
                }
            }
        });

        following_lessons.setItems(Context.getContext().followingLessonsProperty());
        following_lessons.setCellFactory((ListView<FollowingLessonContext> param) -> new ListCell<FollowingLessonContext>() {
            @Override
            protected void updateItem(FollowingLessonContext l_ctx, boolean empty) {
                super.updateItem(l_ctx, empty);
                if (empty) {
                    setText(null);
                } else {
                    setText(l_ctx.getLesson().name);
                }
            }
        });

        teachers.setItems(Context.getContext().teachersProperty());
        teachers.setCellFactory((ListView<TeacherContext> param) -> new ListCell<TeacherContext>() {
            @Override
            protected void updateItem(TeacherContext tch_ctx, boolean empty) {
                super.updateItem(tch_ctx, empty);
                if (empty) {
                    setText(null);
                } else {
                    setText(tch_ctx.getTeacher().first_name + " " + tch_ctx.getTeacher().last_name);
                    if (tch_ctx.isOnline()) {
                        setStyle("-fx-text-fill: black;");
                    } else {
                        setStyle("-fx-text-fill: gray;");
                    }
                }
            }
        });
        add_teachers_button.graphicProperty().set(new Glyph("FontAwesome", FontAwesome.Glyph.PLUS));
        add_teachers_button.disableProperty().bind(user.isNull());
        remove_teachers_button.graphicProperty().set(new Glyph("FontAwesome", FontAwesome.Glyph.MINUS));
        remove_teachers_button.disableProperty().bind(Bindings.isEmpty(teachers.selectionModelProperty().get().getSelectedItems()));

        teach_accord.setExpandedPane(teach_accord.getPanes().get(0));
        teaching_lessons.setItems(Context.getContext().teachingLessonsProperty());
        teaching_lessons.setCellFactory((ListView<TeachingLessonContext> param) -> new ListCell<TeachingLessonContext>() {
            @Override
            protected void updateItem(TeachingLessonContext l_ctx, boolean empty) {
                super.updateItem(l_ctx, empty);
                if (empty) {
                    setText(null);
                } else {
                    setText(l_ctx.getLesson().name);
                }
            }
        });
        add_lesson_button.graphicProperty().set(new Glyph("FontAwesome", FontAwesome.Glyph.PLUS));
        add_lesson_button.disableProperty().bind(user.isNull());
        remove_lessons_button.graphicProperty().set(new Glyph("FontAwesome", FontAwesome.Glyph.MINUS));
        remove_lessons_button.disableProperty().bind(Bindings.isEmpty(teaching_lessons.selectionModelProperty().get().getSelectedItems()));

        students.setItems(Context.getContext().studentsProperty());
        students.setCellFactory((ListView<StudentContext> param) -> new ListCell<StudentContext>() {
            @Override
            protected void updateItem(StudentContext std_ctx, boolean empty) {
                super.updateItem(std_ctx, empty);
                if (empty) {
                    setText(null);
                } else {
                    setText(std_ctx.getStudent().first_name + " " + std_ctx.getStudent().last_name);
                    if (std_ctx.isOnline()) {
                        setStyle("-fx-text-fill: black;");
                    } else {
                        setStyle("-fx-text-fill: gray;");
                    }
                }
            }
        });

        parameters.setItems(Context.getContext().parametersProperty());
        par_names.setCellValueFactory(new PropertyValueFactory<>("name"));
        par_vals.setCellValueFactory(new PropertyValueFactory<>("value"));
        par_vals.setCellFactory(TextFieldTableCell.forTableColumn());
        par_vals.setOnEditCommit((TableColumn.CellEditEvent<ParameterValue, String> event) -> {
            ParameterValue par_value = (ParameterValue) event.getTableView().getItems().get(event.getTablePosition().getRow());
            String[] par_name = par_value.nameProperty().get().split("\\.");
            Context.getContext().setParameterValue(par_name[0], par_name[1], event.getNewValue());
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
                alert.setHeaderText(e.getMessage());
                alert.showAndWait();
            }
        });
    }

    @FXML
    private void logout(ActionEvent event) {
        Context.getContext().logout();
    }

    @FXML
    private void new_user(ActionEvent event) {
        NewUserDialog new_user_dialog = new NewUserDialog();
        new_user_dialog.getDialogPane().getStylesheets().addAll(Context.getContext().getStage().getScene().getStylesheets());
        new_user_dialog.showAndWait().ifPresent(user -> {
            try {
                Context.getContext().newUser(user.getEmail(), user.getPassword(), user.getFirstName(), user.getLastName());
            } catch (Exception e) {
                Alert alert = new Alert(AlertType.ERROR);
                alert.getDialogPane().getStylesheets().addAll(Context.getContext().getStage().getScene().getStylesheets());
                alert.setTitle("Exception");
                alert.setHeaderText(e.getMessage());
                alert.showAndWait();
            }
        }
        );
    }

    @FXML
    private void exit(ActionEvent event) {
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
        teachers.selectionModelProperty().get().getSelectedItems().forEach(tch_ctx -> Context.getContext().removeTeacher(tch_ctx));
    }

    public void add_lesson() {
        new AddLessonDialog().showAndWait().ifPresent(new_lesson -> Context.getContext().addLesson(new_lesson.getLessonName(), new_lesson.getModel(), new_lesson.getRoles()));
    }

    public void remove_selected_lessons() {
        teaching_lessons.selectionModelProperty().get().getSelectedItems().forEach(l_ctx -> Context.getContext().removeLesson(l_ctx));
    }
}
