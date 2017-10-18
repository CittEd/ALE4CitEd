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
import it.cnr.istc.ale.api.model.LessonModel;
import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import javafx.beans.binding.BooleanBinding;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.ButtonType;
import javafx.scene.control.Dialog;
import javafx.scene.control.Label;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.TextField;
import javafx.scene.control.cell.ComboBoxTableCell;
import javafx.scene.control.cell.PropertyValueFactory;
import javafx.scene.layout.GridPane;
import static javafx.scene.layout.GridPane.setHgrow;
import static javafx.scene.layout.GridPane.setVgrow;
import javafx.scene.layout.Priority;
import javafx.stage.FileChooser;
import javafx.util.StringConverter;

/**
 *
 * @author Riccardo De Benedictis <riccardo.debenedictis@istc.cnr.it>
 */
public class AddLessonDialog extends Dialog<AddLessonDialog.AddLessonResult> {

    private static final FileChooser FILE_CHOOSER = new FileChooser();
    private final GridPane grid = new GridPane();
    private final TextField lesson_type_name = new TextField();
    private final TextField lesson_name = new TextField();
    private final Button open_button = new Button("Open");
    private final ObservableList<StudentRole> roles = FXCollections.observableArrayList();
    private final TableView<StudentRole> roles_table_view = new TableView<>(roles);
    private final TableColumn<StudentRole, String> role_column = new TableColumn<>("Role");
    private final TableColumn<StudentRole, User> student_column = new TableColumn<>("Student");
    private final ButtonType add_button = new ButtonType("Add", ButtonBar.ButtonData.OK_DONE);
    private LessonModel lesson_model;

    public AddLessonDialog() {
        grid.setHgap(10);
        grid.setVgap(10);
        setHgrow(lesson_type_name, Priority.ALWAYS);
        setHgrow(lesson_name, Priority.ALWAYS);
        setVgrow(roles_table_view, Priority.ALWAYS);
        setHgrow(roles_table_view, Priority.ALWAYS);

        grid.add(new Label("Lesson type:"), 0, 0);
        lesson_type_name.setPromptText("Lesson type");
        lesson_type_name.setEditable(false);
        grid.add(lesson_type_name, 1, 0);
        grid.add(open_button, 2, 0);
        grid.add(new Label("Lesson name:"), 0, 1);
        lesson_name.setPromptText("Lesson name");
        grid.add(lesson_name, 1, 1, 2, 1);
        grid.add(roles_table_view, 0, 2, 3, 1);

        roles_table_view.getColumns().addAll(role_column, student_column);
        roles_table_view.setEditable(true);

        roles_table_view.setColumnResizePolicy(TableView.CONSTRAINED_RESIZE_POLICY);
        role_column.setCellValueFactory(new PropertyValueFactory("role"));
        student_column.setCellValueFactory(new PropertyValueFactory("student"));
        student_column.setCellFactory(ComboBoxTableCell.forTableColumn(new StringConverter<User>() {
            @Override
            public String toString(User user) {
                if (user == null) {
                    return "Select one";
                } else {
                    return user.getLastName() + " " + user.getFirstName();
                }
            }

            @Override
            public User fromString(String string) {
                throw new UnsupportedOperationException("Not supported yet.");
            }
        }, Context.getContext().getStudents()));
        student_column.setEditable(true);
        student_column.setOnEditCommit((TableColumn.CellEditEvent<StudentRole, User> event) -> {
            roles.get(event.getTablePosition().getRow()).student.set(event.getNewValue());
        });

        getDialogPane().setContent(grid);

        open_button.setOnAction((ActionEvent event) -> {
            FILE_CHOOSER.setTitle("Open Lesson File");
            FILE_CHOOSER.setInitialDirectory(new File(System.getProperty("user.home")));
            FILE_CHOOSER.getExtensionFilters().clear();
            FILE_CHOOSER.getExtensionFilters().addAll(
                    new FileChooser.ExtensionFilter("Lesson", "*.json"),
                    new FileChooser.ExtensionFilter("All Files", "*.*")
            );
            File lesson_file = FILE_CHOOSER.showOpenDialog(Context.getContext().getStage());
            if (lesson_file != null) {
                try {
                    lesson_model = Context.MAPPER.readValue(lesson_file, LessonModel.class);
                    lesson_type_name.setText(lesson_model.getName());
                    for (String role : lesson_model.getRoles()) {
                        roles.add(new StudentRole(role, null));
                    }
                } catch (IOException ex) {
                    Logger.getLogger(AddLessonDialog.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
        });

        getDialogPane().getButtonTypes().add(add_button);
        getDialogPane().lookupButton(add_button).disableProperty().bind(lesson_type_name.textProperty().isEmpty().or(lesson_name.textProperty().isEmpty()).or(new BooleanBinding() {
            @Override
            protected boolean computeValue() {
                return roles.stream().anyMatch(role -> role.student.get() == null);
            }
        }));
        setResultConverter((ButtonType param) -> param == add_button ? new AddLessonResult(lesson_model, lesson_name.getText(), roles.stream().collect(Collectors.toMap(StudentRole::getRole, StudentRole::getStudentId))) : null);
    }

    public static class StudentRole {

        public final StringProperty role;
        public final ObjectProperty<User> student;

        public StudentRole(String name, User student) {
            this.role = new SimpleStringProperty(name);
            this.student = new SimpleObjectProperty<>(student);
        }

        public String getRole() {
            return role.get();
        }

        public StringProperty roleProperty() {
            return role;
        }

        public Long getStudentId() {
            return student.get().getId();
        }

        public ObjectProperty<User> studentProperty() {
            return student;
        }
    }

    public static class AddLessonResult {

        private final LessonModel model;
        private final String lesson_name;
        private final Map<String, Long> roles;

        private AddLessonResult(LessonModel model, String lesson_name, Map<String, Long> roles) {
            this.model = model;
            this.lesson_name = lesson_name;
            this.roles = roles;
        }

        public LessonModel getModel() {
            return model;
        }

        public String getLessonName() {
            return lesson_name;
        }

        public Map<String, Long> getRoles() {
            return Collections.unmodifiableMap(roles);
        }
    }
}
