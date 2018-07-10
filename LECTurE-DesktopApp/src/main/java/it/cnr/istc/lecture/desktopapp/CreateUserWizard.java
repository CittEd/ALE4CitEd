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
    private final QuestionnairePane q0 = new QuestionnairePane("In che modo ritengo di poter contribuire ad un lavoro di gruppo", "Penso di poter capire rapidamente nuove opportunità operativa e trarne vantaggio", "Sono in grado di lavorare bene con le persone più diverse");

    public CreateUserWizard() {
        super(Context.LANGUAGE.getString("NEW USER"));
        setFlow(new Wizard.LinearFlow(cp, q0));

        setResultConverter((ButtonType param) -> {
            if (param.getButtonData() == FINISH) {
                return new NewUserResult(cp.email_field.getText(), cp.password_field.getText(), cp.first_name_field.getText(), cp.last_name_field.getText());
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
                this.spinners[i].setMinHeight(50);
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

        private NewUserResult(String email, String password, String first_name, String last_name) {
            this.email = email;
            this.password = password;
            this.first_name = first_name;
            this.last_name = last_name;
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
    }
}
