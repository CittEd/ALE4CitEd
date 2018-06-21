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
import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;
import java.util.logging.Level;
import java.util.logging.Logger;
import javafx.application.Platform;
import javafx.beans.binding.Bindings;
import javafx.beans.property.ObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ListChangeListener;
import javafx.collections.transformation.SortedList;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
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
import javafx.scene.control.ToggleButton;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.control.cell.TextFieldTableCell;
import javafx.scene.layout.Pane;
import javafx.scene.layout.StackPane;
import javafx.scene.paint.Color;
import javafx.stage.Stage;
import org.controlsfx.control.Notifications;
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
    private Pane text_event_pane;
    private TextEventController text_event_controller;
    private Pane question_event_pane;
    private QuestionEventController question_event_controller;
    private Pane url_event_pane;
    private URLEventController url_event_controller;
    @FXML
    private StackPane learning_pane;
    @FXML
    private Accordion teach_accord;
    @FXML
    private ListView<TeachingLessonContext> teaching_lessons;
    @FXML
    private Button add_lesson_button;
    @FXML
    private Button remove_lessons_button;
    @FXML
    private StackPane teaching_pane;
    private Pane lesson_pane;
    private LessonController lesson_controller;
    private Pane student_pane;
    private StudentController student_controller;
    @FXML
    private ToggleButton simulate_data;
    @FXML
    private ListView<StudentContext> students;
    @FXML
    private TableView<ParameterValue> parameters;
    @FXML
    private TableColumn<ParameterValue, String> par_names;
    @FXML
    private TableColumn<ParameterValue, String> par_vals;
    private final RandomDataGenerator random_data_generator = new RandomDataGenerator();

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
                learning_pane.getChildren().clear();
                teaching_pane.getChildren().clear();
            }
        });

        login.disableProperty().bind(user.isNotNull());
        new_user.disableProperty().bind(user.isNotNull());
        logout.disableProperty().bind(user.isNull());

        learn_accord.setExpandedPane(learn_accord.getPanes().get(0));

        events.setItems(new SortedList<>(Context.getContext().eventsProperty(), (Event e0, Event e1) -> Long.compare(e0.time, e1.time)));
        events.setCellFactory((ListView<Event> param) -> new ListCell<Event>() {
            @Override
            protected void updateItem(Event event, boolean empty) {
                super.updateItem(event, empty);
                if (empty) {
                    setText(null);
                    setGraphic(null);
                } else {
                    if (event instanceof TextEvent) {
                        setText(((TextEvent) event).content);
                        setGraphic(new Glyph("FontAwesome", FontAwesome.Glyph.INFO));
                    } else if (event instanceof URLEvent) {
                        setText(((URLEvent) event).content);
                        setGraphic(new Glyph("FontAwesome", FontAwesome.Glyph.INFO));
                    } else if (event instanceof QuestionEvent) {
                        setText(((QuestionEvent) event).question);
                        setGraphic(new Glyph("FontAwesome", FontAwesome.Glyph.QUESTION));
                    }
                }
            }
        });
        Context.getContext().eventsProperty().addListener((ListChangeListener.Change<? extends Event> c) -> {
            while (c.next()) {
                for (Event event : c.getAddedSubList()) {
                    switch (event.event_type) {
                        case TextEvent:
                            Platform.runLater(() -> Notifications.create().title(Context.LANGUAGE.getString("EVENT")).text(((TextEvent) event).content).show());
                            break;
                        case QuestionEvent:
                            Platform.runLater(() -> Notifications.create().title(Context.LANGUAGE.getString("QUESTION")).text(((QuestionEvent) event).question).show());
                            break;
                        case URLEvent:
                            Platform.runLater(() -> Notifications.create().title(Context.LANGUAGE.getString("EVENT")).text(((URLEvent) event).content).show());
                            break;
                        default:
                            throw new AssertionError(event.event_type.name());
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
                    setGraphic(null);
                } else {
                    setText(l_ctx.getLesson().name);
                    switch (l_ctx.stateProperty().get()) {
                        case Running:
                            setGraphic(new Glyph("FontAwesome", FontAwesome.Glyph.PLAY).color(Color.INDIGO));
                            break;
                        case Paused:
                            setGraphic(new Glyph("FontAwesome", FontAwesome.Glyph.PAUSE).color(Color.INDIGO));
                            break;
                        case Stopped:
                            setGraphic(new Glyph("FontAwesome", FontAwesome.Glyph.STOP).color(Color.INDIGO));
                            break;
                        default:
                            throw new AssertionError(l_ctx.stateProperty().get().name());
                    }
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
                    setGraphic(null);
                } else {
                    setText(tch_ctx.getTeacher().first_name + " " + tch_ctx.getTeacher().last_name);
                    if (tch_ctx.isOnline()) {
                        setStyle("-fx-text-fill: black;");
                        setGraphic(new Glyph("FontAwesome", FontAwesome.Glyph.LINK));
                    } else {
                        setStyle("-fx-text-fill: gray;");
                        setGraphic(new Glyph("FontAwesome", FontAwesome.Glyph.UNLINK));
                    }
                }
            }
        });
        add_teachers_button.graphicProperty().set(new Glyph("FontAwesome", FontAwesome.Glyph.PLUS));
        add_teachers_button.disableProperty().bind(user.isNull());
        remove_teachers_button.graphicProperty().set(new Glyph("FontAwesome", FontAwesome.Glyph.MINUS));
        remove_teachers_button.disableProperty().bind(Bindings.isEmpty(teachers.selectionModelProperty().get().getSelectedItems()));

        try {
            FXMLLoader text_event_pane_loader = new FXMLLoader(getClass().getResource("/fxml/TextEvent.fxml"));
            text_event_pane = text_event_pane_loader.load();
            text_event_controller = text_event_pane_loader.getController();
            FXMLLoader question_event_pane_loader = new FXMLLoader(getClass().getResource("/fxml/QuestionEvent.fxml"));
            question_event_pane = question_event_pane_loader.load();
            question_event_controller = question_event_pane_loader.getController();
            FXMLLoader url_event_pane_loader = new FXMLLoader(getClass().getResource("/fxml/URLEvent.fxml"));
            url_event_pane = url_event_pane_loader.load();
            url_event_controller = url_event_pane_loader.getController();
        } catch (IOException ex) {
            Logger.getLogger(MainController.class.getName()).log(Level.SEVERE, null, ex);
        }
        events.getSelectionModel().selectedItemProperty().addListener((ObservableValue<? extends Event> observable, Event oldValue, Event newValue) -> {
            if (newValue != null) {
                Pane c_pane = null;
                switch (newValue.event_type) {
                    case TextEvent:
                        c_pane = text_event_pane;
                        text_event_controller.eventProperty().set((TextEvent) newValue);
                        break;
                    case QuestionEvent:
                        c_pane = question_event_pane;
                        question_event_controller.eventProperty().set((QuestionEvent) newValue);
                        break;
                    case URLEvent:
                        c_pane = url_event_pane;
                        url_event_controller.eventProperty().set((URLEvent) newValue);
                        break;
                    default:
                        throw new AssertionError(newValue.event_type.name());
                }
                if (learning_pane.getChildren().isEmpty()) {
                    learning_pane.getChildren().add(c_pane);
                } else if (learning_pane.getChildren().get(0) != c_pane) {
                    learning_pane.getChildren().set(0, c_pane);
                }
            } else {
                text_event_controller.eventProperty().set(null);
                url_event_controller.eventProperty().set(null);
            }
        });

        teach_accord.setExpandedPane(teach_accord.getPanes().get(0));
        teaching_lessons.setItems(Context.getContext().teachingLessonsProperty());
        teaching_lessons.setCellFactory((ListView<TeachingLessonContext> param) -> new ListCell<TeachingLessonContext>() {
            @Override
            protected void updateItem(TeachingLessonContext l_ctx, boolean empty) {
                super.updateItem(l_ctx, empty);
                if (empty) {
                    setText(null);
                    setGraphic(null);
                } else {
                    setText(l_ctx.getLesson().name);
                    switch (l_ctx.stateProperty().get()) {
                        case Running:
                            setGraphic(new Glyph("FontAwesome", FontAwesome.Glyph.PLAY).color(Color.INDIGO));
                            break;
                        case Paused:
                            setGraphic(new Glyph("FontAwesome", FontAwesome.Glyph.PAUSE).color(Color.INDIGO));
                            break;
                        case Stopped:
                            setGraphic(new Glyph("FontAwesome", FontAwesome.Glyph.STOP).color(Color.INDIGO));
                            break;
                        default:
                            throw new AssertionError(l_ctx.stateProperty().get().name());
                    }
                }
            }
        });
        add_lesson_button.graphicProperty().set(new Glyph("FontAwesome", FontAwesome.Glyph.PLUS));
        add_lesson_button.disableProperty().bind(user.isNull());
        remove_lessons_button.graphicProperty().set(new Glyph("FontAwesome", FontAwesome.Glyph.MINUS));
        remove_lessons_button.disableProperty().bind(Bindings.isEmpty(teaching_lessons.selectionModelProperty().get().getSelectedItems()));

        try {
            FXMLLoader lesson_pane_loader = new FXMLLoader(getClass().getResource("/fxml/Lesson.fxml"), Context.LANGUAGE);
            lesson_pane = lesson_pane_loader.load();
            lesson_controller = lesson_pane_loader.getController();
        } catch (IOException ex) {
            Logger.getLogger(MainController.class.getName()).log(Level.SEVERE, null, ex);
        }

        teaching_lessons.getSelectionModel().selectedItemProperty().addListener((ObservableValue<? extends TeachingLessonContext> observable, TeachingLessonContext oldValue, TeachingLessonContext newValue) -> {
            lesson_controller.teachingLessonContextProperty().set(newValue);
            if (newValue != null) {
                if (teaching_pane.getChildren().isEmpty()) {
                    teaching_pane.getChildren().add(lesson_pane);
                } else if (teaching_pane.getChildren().get(0) != lesson_pane) {
                    teaching_pane.getChildren().set(0, lesson_pane);
                }
            }
        });

        students.setItems(Context.getContext().studentsProperty());
        students.setCellFactory((ListView<StudentContext> param) -> new ListCell<StudentContext>() {
            @Override
            protected void updateItem(StudentContext std_ctx, boolean empty) {
                super.updateItem(std_ctx, empty);
                if (empty) {
                    setText(null);
                    setGraphic(null);
                } else {
                    setText(std_ctx.getStudent().first_name + " " + std_ctx.getStudent().last_name);
                    if (std_ctx.isOnline()) {
                        setStyle("-fx-text-fill: black;");
                        setGraphic(new Glyph("FontAwesome", FontAwesome.Glyph.LINK));
                    } else {
                        setStyle("-fx-text-fill: gray;");
                        setGraphic(new Glyph("FontAwesome", FontAwesome.Glyph.UNLINK));
                    }
                }
            }
        });

        try {
            FXMLLoader student_pane_loader = new FXMLLoader(getClass().getResource("/fxml/Student.fxml"), Context.LANGUAGE);
            student_pane = student_pane_loader.load();
            student_controller = student_pane_loader.getController();
        } catch (IOException ex) {
            Logger.getLogger(MainController.class.getName()).log(Level.SEVERE, null, ex);
        }

        students.getSelectionModel().selectedItemProperty().addListener((ObservableValue<? extends StudentContext> observable, StudentContext oldValue, StudentContext newValue) -> {
            student_controller.studentContextProperty().set(newValue);
            if (newValue != null) {
                if (teaching_pane.getChildren().isEmpty()) {
                    teaching_pane.getChildren().add(student_pane);
                } else if (teaching_pane.getChildren().get(0) != student_pane) {
                    teaching_pane.getChildren().set(0, student_pane);
                }
            }
        });

        simulate_data.graphicProperty().set(new Glyph("FontAwesome", FontAwesome.Glyph.RANDOM));
        simulate_data.disableProperty().bind(Context.getContext().userProperty().isNull());
        simulate_data.selectedProperty().addListener(random_data_generator);

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
                alert.setTitle(Context.LANGUAGE.getString("EXCEPTION"));
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
                alert.setTitle(Context.LANGUAGE.getString("EXCEPTION"));
                alert.setHeaderText(e.getMessage());
                alert.showAndWait();
            }
        });
    }

    @FXML
    private void exit(ActionEvent event) {
        random_data_generator.shutdown();
        Platform.exit();
    }

    @FXML
    private void add_teachers(ActionEvent event) {
        new AddTeachersDialog().showAndWait().ifPresent(teachers_to_add -> {
            for (User teacher : teachers_to_add) {
                Context.getContext().addTeacher(teacher);
            }
        });
    }

    @FXML
    private void remove_selected_teachers(ActionEvent event) {
        teachers.selectionModelProperty().get().getSelectedItems().forEach(tch_ctx -> Context.getContext().removeTeacher(tch_ctx));
    }

    @FXML
    private void add_lesson(ActionEvent event) {
        new AddLessonDialog().showAndWait().ifPresent(new_lesson -> Context.getContext().addLesson(new_lesson.getLessonName(), new_lesson.getModel(), new_lesson.getRoles()));
    }

    @FXML
    private void remove_selected_lessons(ActionEvent event) {
        teaching_lessons.selectionModelProperty().get().getSelectedItems().forEach(l_ctx -> Context.getContext().removeLesson(l_ctx));
    }
}
