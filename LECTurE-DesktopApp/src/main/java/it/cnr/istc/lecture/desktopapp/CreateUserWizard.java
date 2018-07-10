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
                return new NewUserResult(
                        cp.email_field.getText(),
                        cp.password_field.getText(),
                        cp.first_name_field.getText(),
                        cp.last_name_field.getText(),
                        (int) q1.spinners[0].valueProperty().get(),
                        (int) q1.spinners[1].valueProperty().get(),
                        (int) q1.spinners[2].valueProperty().get(),
                        (int) q1.spinners[3].valueProperty().get(),
                        (int) q1.spinners[4].valueProperty().get(),
                        (int) q1.spinners[5].valueProperty().get(),
                        (int) q1.spinners[6].valueProperty().get(),
                        (int) q1.spinners[7].valueProperty().get(),
                        (int) q2.spinners[0].valueProperty().get(),
                        (int) q2.spinners[1].valueProperty().get(),
                        (int) q2.spinners[2].valueProperty().get(),
                        (int) q2.spinners[3].valueProperty().get(),
                        (int) q2.spinners[4].valueProperty().get(),
                        (int) q2.spinners[5].valueProperty().get(),
                        (int) q3.spinners[0].valueProperty().get(),
                        (int) q3.spinners[1].valueProperty().get(),
                        (int) q3.spinners[2].valueProperty().get(),
                        (int) q3.spinners[3].valueProperty().get(),
                        (int) q3.spinners[4].valueProperty().get(),
                        (int) q3.spinners[5].valueProperty().get(),
                        (int) q3.spinners[6].valueProperty().get(),
                        (int) q3.spinners[7].valueProperty().get(),
                        (int) q4.spinners[0].valueProperty().get(),
                        (int) q4.spinners[1].valueProperty().get(),
                        (int) q4.spinners[2].valueProperty().get(),
                        (int) q4.spinners[3].valueProperty().get(),
                        (int) q4.spinners[4].valueProperty().get(),
                        (int) q4.spinners[5].valueProperty().get(),
                        (int) q4.spinners[6].valueProperty().get(),
                        (int) q4.spinners[7].valueProperty().get(),
                        (int) q5.spinners[0].valueProperty().get(),
                        (int) q5.spinners[1].valueProperty().get(),
                        (int) q5.spinners[2].valueProperty().get(),
                        (int) q5.spinners[3].valueProperty().get(),
                        (int) q5.spinners[4].valueProperty().get(),
                        (int) q5.spinners[5].valueProperty().get(),
                        (int) q5.spinners[6].valueProperty().get(),
                        (int) q5.spinners[7].valueProperty().get(),
                        (int) q6.spinners[0].valueProperty().get(),
                        (int) q6.spinners[1].valueProperty().get(),
                        (int) q6.spinners[2].valueProperty().get(),
                        (int) q6.spinners[3].valueProperty().get(),
                        (int) q6.spinners[4].valueProperty().get(),
                        (int) q6.spinners[5].valueProperty().get(),
                        (int) q6.spinners[6].valueProperty().get(),
                        (int) q6.spinners[7].valueProperty().get(),
                        (int) q7.spinners[0].valueProperty().get(),
                        (int) q7.spinners[1].valueProperty().get(),
                        (int) q7.spinners[2].valueProperty().get(),
                        (int) q7.spinners[3].valueProperty().get(),
                        (int) q7.spinners[4].valueProperty().get(),
                        (int) q7.spinners[5].valueProperty().get(),
                        (int) q7.spinners[6].valueProperty().get(),
                        (int) q7.spinners[7].valueProperty().get());
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
        private final int qs1a;
        private final int qs1b;
        private final int qs1c;
        private final int qs1d;
        private final int qs1e;
        private final int qs1f;
        private final int qs1g;
        private final int qs1h;
        private final int qs2a;
        private final int qs2b;
        private final int qs2c;
        private final int qs2d;
        private final int qs2e;
        private final int qs2f;
        private final int qs3a;
        private final int qs3b;
        private final int qs3c;
        private final int qs3d;
        private final int qs3e;
        private final int qs3f;
        private final int qs3g;
        private final int qs3h;
        private final int qs4a;
        private final int qs4b;
        private final int qs4c;
        private final int qs4d;
        private final int qs4e;
        private final int qs4f;
        private final int qs4g;
        private final int qs4h;
        private final int qs5a;
        private final int qs5b;
        private final int qs5c;
        private final int qs5d;
        private final int qs5e;
        private final int qs5f;
        private final int qs5g;
        private final int qs5h;
        private final int qs6a;
        private final int qs6b;
        private final int qs6c;
        private final int qs6d;
        private final int qs6e;
        private final int qs6f;
        private final int qs6g;
        private final int qs6h;
        private final int qs7a;
        private final int qs7b;
        private final int qs7c;
        private final int qs7d;
        private final int qs7e;
        private final int qs7f;
        private final int qs7g;
        private final int qs7h;

        public NewUserResult(String email, String password, String first_name, String last_name, int qs1a, int qs1b, int qs1c, int qs1d, int qs1e, int qs1f, int qs1g, int qs1h, int qs2a, int qs2b, int qs2c, int qs2d, int qs2e, int qs2f, int qs3a, int qs3b, int qs3c, int qs3d, int qs3e, int qs3f, int qs3g, int qs3h, int qs4a, int qs4b, int qs4c, int qs4d, int qs4e, int qs4f, int qs4g, int qs4h, int qs5a, int qs5b, int qs5c, int qs5d, int qs5e, int qs5f, int qs5g, int qs5h, int qs6a, int qs6b, int qs6c, int qs6d, int qs6e, int qs6f, int qs6g, int qs6h, int qs7a, int qs7b, int qs7c, int qs7d, int qs7e, int qs7f, int qs7g, int qs7h) {
            this.email = email;
            this.password = password;
            this.first_name = first_name;
            this.last_name = last_name;
            this.qs1a = qs1a;
            this.qs1b = qs1b;
            this.qs1c = qs1c;
            this.qs1d = qs1d;
            this.qs1e = qs1e;
            this.qs1f = qs1f;
            this.qs1g = qs1g;
            this.qs1h = qs1h;
            this.qs2a = qs2a;
            this.qs2b = qs2b;
            this.qs2c = qs2c;
            this.qs2d = qs2d;
            this.qs2e = qs2e;
            this.qs2f = qs2f;
            this.qs3a = qs3a;
            this.qs3b = qs3b;
            this.qs3c = qs3c;
            this.qs3d = qs3d;
            this.qs3e = qs3e;
            this.qs3f = qs3f;
            this.qs3g = qs3g;
            this.qs3h = qs3h;
            this.qs4a = qs4a;
            this.qs4b = qs4b;
            this.qs4c = qs4c;
            this.qs4d = qs4d;
            this.qs4e = qs4e;
            this.qs4f = qs4f;
            this.qs4g = qs4g;
            this.qs4h = qs4h;
            this.qs5a = qs5a;
            this.qs5b = qs5b;
            this.qs5c = qs5c;
            this.qs5d = qs5d;
            this.qs5e = qs5e;
            this.qs5f = qs5f;
            this.qs5g = qs5g;
            this.qs5h = qs5h;
            this.qs6a = qs6a;
            this.qs6b = qs6b;
            this.qs6c = qs6c;
            this.qs6d = qs6d;
            this.qs6e = qs6e;
            this.qs6f = qs6f;
            this.qs6g = qs6g;
            this.qs6h = qs6h;
            this.qs7a = qs7a;
            this.qs7b = qs7b;
            this.qs7c = qs7c;
            this.qs7d = qs7d;
            this.qs7e = qs7e;
            this.qs7f = qs7f;
            this.qs7g = qs7g;
            this.qs7h = qs7h;
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

        public int getQs1a() {
            return qs1a;
        }

        public int getQs1b() {
            return qs1b;
        }

        public int getQs1c() {
            return qs1c;
        }

        public int getQs1d() {
            return qs1d;
        }

        public int getQs1e() {
            return qs1e;
        }

        public int getQs1f() {
            return qs1f;
        }

        public int getQs1g() {
            return qs1g;
        }

        public int getQs1h() {
            return qs1h;
        }

        public int getQs2a() {
            return qs2a;
        }

        public int getQs2b() {
            return qs2b;
        }

        public int getQs2c() {
            return qs2c;
        }

        public int getQs2d() {
            return qs2d;
        }

        public int getQs2e() {
            return qs2e;
        }

        public int getQs2f() {
            return qs2f;
        }

        public int getQs3a() {
            return qs3a;
        }

        public int getQs3b() {
            return qs3b;
        }

        public int getQs3c() {
            return qs3c;
        }

        public int getQs3d() {
            return qs3d;
        }

        public int getQs3e() {
            return qs3e;
        }

        public int getQs3f() {
            return qs3f;
        }

        public int getQs3g() {
            return qs3g;
        }

        public int getQs3h() {
            return qs3h;
        }

        public int getQs4a() {
            return qs4a;
        }

        public int getQs4b() {
            return qs4b;
        }

        public int getQs4c() {
            return qs4c;
        }

        public int getQs4d() {
            return qs4d;
        }

        public int getQs4e() {
            return qs4e;
        }

        public int getQs4f() {
            return qs4f;
        }

        public int getQs4g() {
            return qs4g;
        }

        public int getQs4h() {
            return qs4h;
        }

        public int getQs5a() {
            return qs5a;
        }

        public int getQs5b() {
            return qs5b;
        }

        public int getQs5c() {
            return qs5c;
        }

        public int getQs5d() {
            return qs5d;
        }

        public int getQs5e() {
            return qs5e;
        }

        public int getQs5f() {
            return qs5f;
        }

        public int getQs5g() {
            return qs5g;
        }

        public int getQs5h() {
            return qs5h;
        }

        public int getQs6a() {
            return qs6a;
        }

        public int getQs6b() {
            return qs6b;
        }

        public int getQs6c() {
            return qs6c;
        }

        public int getQs6d() {
            return qs6d;
        }

        public int getQs6e() {
            return qs6e;
        }

        public int getQs6f() {
            return qs6f;
        }

        public int getQs6g() {
            return qs6g;
        }

        public int getQs6h() {
            return qs6h;
        }

        public int getQs7a() {
            return qs7a;
        }

        public int getQs7b() {
            return qs7b;
        }

        public int getQs7c() {
            return qs7c;
        }

        public int getQs7d() {
            return qs7d;
        }

        public int getQs7e() {
            return qs7e;
        }

        public int getQs7f() {
            return qs7f;
        }

        public int getQs7g() {
            return qs7g;
        }

        public int getQs7h() {
            return qs7h;
        }
    }
}
