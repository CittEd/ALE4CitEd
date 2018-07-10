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

import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.geometry.Insets;
import static javafx.scene.control.ButtonBar.ButtonData.FINISH;
import javafx.scene.control.ButtonType;
import javafx.scene.control.Label;
import javafx.scene.control.PasswordField;
import javafx.scene.control.Spinner;
import javafx.scene.control.SpinnerValueFactory;
import javafx.scene.control.TextField;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;

/**
 *
 * @author Riccardo De Benedictis
 */
public class CreateUserWizard extends Wizard<CreateUserWizard.NewUserResult> {

    private final CredentialsPane cp = new CredentialsPane();
    private final QuestionnairePane q1 = new QuestionnairePane(Context.LANGUAGE.getString("QS1"), Context.LANGUAGE.getString("QS1a"), Context.LANGUAGE.getString("QS1b"), Context.LANGUAGE.getString("QS1c"), Context.LANGUAGE.getString("QS1d"), Context.LANGUAGE.getString("QS1e"), Context.LANGUAGE.getString("QS1f"), Context.LANGUAGE.getString("QS1g"), Context.LANGUAGE.getString("QS1h"));
    private final QuestionnairePane q2 = new QuestionnairePane(Context.LANGUAGE.getString("QS2"), Context.LANGUAGE.getString("QS2a"), Context.LANGUAGE.getString("QS2b"), Context.LANGUAGE.getString("QS2c"), Context.LANGUAGE.getString("QS2d"), Context.LANGUAGE.getString("QS2e"), Context.LANGUAGE.getString("QS2f"));
    private final QuestionnairePane q3 = new QuestionnairePane(Context.LANGUAGE.getString("QS3"), Context.LANGUAGE.getString("QS3a"), Context.LANGUAGE.getString("QS3b"), Context.LANGUAGE.getString("QS3c"), Context.LANGUAGE.getString("QS3d"), Context.LANGUAGE.getString("QS3e"), Context.LANGUAGE.getString("QS3f"), Context.LANGUAGE.getString("QS3g"), Context.LANGUAGE.getString("QS3h"));
    private final QuestionnairePane q4 = new QuestionnairePane(Context.LANGUAGE.getString("QS4"), Context.LANGUAGE.getString("QS4a"), Context.LANGUAGE.getString("QS4b"), Context.LANGUAGE.getString("QS4c"), Context.LANGUAGE.getString("QS4d"), Context.LANGUAGE.getString("QS4e"), Context.LANGUAGE.getString("QS4f"), Context.LANGUAGE.getString("QS4g"), Context.LANGUAGE.getString("QS4h"));
    private final QuestionnairePane q5 = new QuestionnairePane(Context.LANGUAGE.getString("QS5"), Context.LANGUAGE.getString("QS5a"), Context.LANGUAGE.getString("QS5b"), Context.LANGUAGE.getString("QS5c"), Context.LANGUAGE.getString("QS5d"), Context.LANGUAGE.getString("QS5e"), Context.LANGUAGE.getString("QS5f"), Context.LANGUAGE.getString("QS5g"), Context.LANGUAGE.getString("QS5h"));
    private final QuestionnairePane q6 = new QuestionnairePane(Context.LANGUAGE.getString("QS6"), Context.LANGUAGE.getString("QS6a"), Context.LANGUAGE.getString("QS6b"), Context.LANGUAGE.getString("QS6c"), Context.LANGUAGE.getString("QS6d"), Context.LANGUAGE.getString("QS6e"), Context.LANGUAGE.getString("QS6f"), Context.LANGUAGE.getString("QS6g"), Context.LANGUAGE.getString("QS6h"));
    private final QuestionnairePane q7 = new QuestionnairePane(Context.LANGUAGE.getString("QS7"), Context.LANGUAGE.getString("QS7a"), Context.LANGUAGE.getString("QS7b"), Context.LANGUAGE.getString("QS7c"), Context.LANGUAGE.getString("QS7d"), Context.LANGUAGE.getString("QS7e"), Context.LANGUAGE.getString("QS7f"), Context.LANGUAGE.getString("QS7g"), Context.LANGUAGE.getString("QS7h"));

    public CreateUserWizard() {
        super(Context.LANGUAGE.getString("NEW USER"));
        setFlow(new Wizard.LinearFlow(cp, q1, q2, q3, q4, q5, q6, q7));

        setResultConverter((ButtonType param) -> {
            if (param.getButtonData() == FINISH) {
                final int concrete = (int) q1.spinners[6].valueProperty().get() + (int) q2.spinners[0].valueProperty().get() + (int) q3.spinners[7].valueProperty().get() + (int) q4.spinners[3].valueProperty().get() + (int) q5.spinners[1].valueProperty().get() + (int) q6.spinners[5].valueProperty().get() + (int) q7.spinners[4].valueProperty().get();
                final int president = (int) q1.spinners[3].valueProperty().get() + (int) q2.spinners[1].valueProperty().get() + (int) q3.spinners[0].valueProperty().get() + (int) q4.spinners[7].valueProperty().get() + (int) q5.spinners[5].valueProperty().get() + (int) q6.spinners[2].valueProperty().get() + (int) q7.spinners[6].valueProperty().get();
                final int structurer = (int) q1.spinners[5].valueProperty().get() + (int) q2.spinners[4].valueProperty().get() + (int) q3.spinners[2].valueProperty().get() + (int) q4.spinners[1].valueProperty().get() + (int) q5.spinners[3].valueProperty().get() + (int) q6.spinners[6].valueProperty().get() + (int) q7.spinners[0].valueProperty().get();
                final int ingenious = (int) q1.spinners[2].valueProperty().get() + 0 + (int) q3.spinners[3].valueProperty().get() + (int) q4.spinners[4].valueProperty().get() + (int) q5.spinners[7].valueProperty().get() + (int) q6.spinners[0].valueProperty().get() + (int) q7.spinners[5].valueProperty().get();
                final int explorer = (int) q1.spinners[0].valueProperty().get() + (int) q2.spinners[2].valueProperty().get() + (int) q3.spinners[5].valueProperty().get() + (int) q4.spinners[6].valueProperty().get() + (int) q5.spinners[4].valueProperty().get() + (int) q6.spinners[7].valueProperty().get() + (int) q7.spinners[3].valueProperty().get();
                final int evaluator = (int) q1.spinners[7].valueProperty().get() + (int) q2.spinners[3].valueProperty().get() + (int) q3.spinners[6].valueProperty().get() + (int) q4.spinners[2].valueProperty().get() + (int) q5.spinners[0].valueProperty().get() + (int) q6.spinners[4].valueProperty().get() + (int) q7.spinners[1].valueProperty().get();
                final int worker = (int) q1.spinners[1].valueProperty().get() + (int) q2.spinners[5].valueProperty().get() + (int) q3.spinners[4].valueProperty().get() + (int) q4.spinners[0].valueProperty().get() + (int) q5.spinners[2].valueProperty().get() + (int) q6.spinners[1].valueProperty().get() + (int) q7.spinners[7].valueProperty().get();
                final int objectivist = (int) q1.spinners[4].valueProperty().get() + 0 + (int) q3.spinners[1].valueProperty().get() + (int) q4.spinners[5].valueProperty().get() + (int) q5.spinners[6].valueProperty().get() + (int) q6.spinners[3].valueProperty().get() + (int) q7.spinners[2].valueProperty().get();
                return new NewUserResult(
                        cp.email_field.getText(),
                        cp.password_field.getText(),
                        cp.first_name_field.getText(),
                        cp.last_name_field.getText(),
                        concrete,
                        president,
                        structurer,
                        ingenious,
                        explorer,
                        evaluator,
                        worker,
                        objectivist
                );
            } else {
                return null;
            }
        });
    }

    private static class CredentialsPane extends WizardPane {

        private final GridPane grid = new GridPane();
        private final TextField email_field = new TextField();
        private final PasswordField password_field = new PasswordField();
        private final TextField first_name_field = new TextField();
        private final TextField last_name_field = new TextField();

        public CredentialsPane() {
            grid.setHgap(10);
            grid.setVgap(10);
            email_field.setPromptText(Context.LANGUAGE.getString("E-MAIL"));
            grid.add(new Label(Context.LANGUAGE.getString("E-MAIL") + ":"), 0, 0);
            grid.add(email_field, 1, 0);
            password_field.setPromptText(Context.LANGUAGE.getString("PASSWORD"));
            grid.add(new Label(Context.LANGUAGE.getString("PASSWORD") + ":"), 0, 1);
            grid.add(password_field, 1, 1);
            first_name_field.setPromptText(Context.LANGUAGE.getString("FIRST NAME"));
            grid.add(new Label(Context.LANGUAGE.getString("FIRST NAME") + ":"), 0, 2);
            grid.add(first_name_field, 1, 2);
            grid.add(new Label(Context.LANGUAGE.getString("LAST NAME") + ":"), 0, 3);
            last_name_field.setPromptText(Context.LANGUAGE.getString("LAST NAME"));
            grid.add(last_name_field, 1, 3);

            ColumnConstraints column1 = new ColumnConstraints();
            column1.setHgrow(Priority.NEVER);
            ColumnConstraints column2 = new ColumnConstraints();
            column2.setHgrow(Priority.ALWAYS);
            grid.getColumnConstraints().addAll(column1, column2);
            grid.setPadding(new Insets(15, 12, 15, 12));

            setContent(grid);
            getStylesheets().addAll(Context.getContext().getStage().getScene().getStylesheets());
        }
    }

    private static class QuestionnairePane extends WizardPane implements ChangeListener<Integer> {

        private final GridPane grid = new GridPane();
        private final Label title;
        private final Label[] items;
        private final Spinner[] spinners;
        private int c_amount = 0;

        private QuestionnairePane(String title, String... items) {
            grid.setHgap(10);
            grid.setVgap(10);

            this.title = new Label(title);
            grid.add(this.title, 0, 0, 2, 1);
            this.items = new Label[items.length];
            this.spinners = new Spinner[items.length];
            for (int i = 0; i < items.length; i++) {
                this.items[i] = new Label(items[i]);
                grid.add(this.items[i], 0, i + 1);
                this.spinners[i] = new Spinner<>(new SpinnerValueFactory.IntegerSpinnerValueFactory(0, 10));
                this.spinners[i].getStyleClass().add(Spinner.STYLE_CLASS_ARROWS_ON_RIGHT_HORIZONTAL);
                this.spinners[i].valueProperty().addListener(this);
                this.spinners[i].setPrefWidth(150);
                grid.add(this.spinners[i], 1, i + 1);
            }

            ColumnConstraints column1 = new ColumnConstraints();
            column1.setHgrow(Priority.ALWAYS);
            ColumnConstraints column2 = new ColumnConstraints();
            column2.setHgrow(Priority.NEVER);
            grid.getColumnConstraints().addAll(column1, column2);
            setPadding(new Insets(15, 12, 15, 12));

            setContent(grid);
            getStylesheets().addAll(Context.getContext().getStage().getScene().getStylesheets());
        }

        @Override
        public void changed(ObservableValue<? extends Integer> observable, Integer oldValue, Integer newValue) {
            c_amount -= oldValue;
            c_amount += newValue;
            if (c_amount == 10) {
                for (Spinner spinner : spinners) {
                    ((SpinnerValueFactory.IntegerSpinnerValueFactory) spinner.valueFactoryProperty().get()).setMax((Integer) spinner.valueProperty().get());
                }
            } else {
                for (Spinner spinner : spinners) {
                    ((SpinnerValueFactory.IntegerSpinnerValueFactory) spinner.valueFactoryProperty().get()).setMax(10);
                }
            }
        }
    }

    public static class NewUserResult {

        private final String email;
        private final String password;
        private final String first_name;
        private final String last_name;
        private final int concrete;
        private final int president;
        private final int structurer;
        private final int ingenious;
        private final int explorer;
        private final int evaluator;
        private final int worker;
        private final int objectivist;

        public NewUserResult(String email, String password, String first_name, String last_name, int concrete, int president, int structurer, int ingenious, int explorer, int evaluator, int worker, int objectivist) {
            this.email = email;
            this.password = password;
            this.first_name = first_name;
            this.last_name = last_name;
            this.concrete = concrete;
            this.president = president;
            this.structurer = structurer;
            this.ingenious = ingenious;
            this.explorer = explorer;
            this.evaluator = evaluator;
            this.worker = worker;
            this.objectivist = objectivist;
        }

        public String getEmail() {
            return email;
        }

        public String getPassword() {
            return password;
        }

        public String getFirstName() {
            return first_name;
        }

        public String getLastName() {
            return last_name;
        }

        public int getConcrete() {
            return concrete;
        }

        public int getPresident() {
            return president;
        }

        public int getStructurer() {
            return structurer;
        }

        public int getIngenious() {
            return ingenious;
        }

        public int getExplorer() {
            return explorer;
        }

        public int getEvaluator() {
            return evaluator;
        }

        public int getWorker() {
            return worker;
        }

        public int getObjectivist() {
            return objectivist;
        }
    }
}
