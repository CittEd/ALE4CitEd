/*
 * Copyright (C) 2018 Riccardo De Benedictis
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

import it.cnr.istc.ale.api.messages.QuestionEvent;
import it.cnr.istc.ale.client.context.Context;
import java.util.List;
import javafx.event.ActionEvent;
import javafx.geometry.HPos;
import javafx.geometry.Insets;
import javafx.scene.control.Button;
import javafx.scene.control.RadioButton;
import javafx.scene.control.TextArea;
import javafx.scene.control.ToggleGroup;
import javafx.scene.layout.GridPane;
import static javafx.scene.layout.GridPane.setHgrow;
import static javafx.scene.layout.GridPane.setVgrow;
import javafx.scene.layout.Priority;

/**
 *
 * @author Riccardo De Benedictis
 */
public class QuestionEventGrid extends GridPane {

    private QuestionEvent event;
    private final TextArea content = new TextArea();
    private final ToggleGroup group = new ToggleGroup();
    private final Button send_answer = new Button("Send");

    public QuestionEventGrid() {
        setHgap(10);
        setVgap(10);
        setPadding(new Insets(10));
        setHgrow(content, Priority.ALWAYS);
        setVgrow(content, Priority.ALWAYS);

        content.setEditable(false);
        add(content, 0, 0);

        send_answer.setOnAction((ActionEvent event1) -> {
            Context.getContext().getLearningContext().answerQuestion(event, group.getToggles().indexOf(group.getSelectedToggle()));
            send_answer.disableProperty().unbind();
            getChildren().stream().filter(chld -> chld != content).forEach(chld -> chld.disableProperty().set(true));
        });

        setHalignment(send_answer, HPos.RIGHT);
    }

    public void setEvent(QuestionEvent event) {
        this.event = event;
        send_answer.disableProperty().unbind();
        group.getToggles().clear();
        content.setText(event.getQuestion());
        getChildren().removeIf(chld -> chld != content);
        List<String> answers = event.getAnswers();
        for (int i = 0; i < answers.size(); i++) {
            RadioButton ans = new RadioButton(answers.get(i));
            ans.setToggleGroup(group);
            setHgrow(ans, Priority.ALWAYS);
            add(ans, 0, i + 1);
            if (event.getAnswer() != null) {
                ans.disableProperty().set(true);
            }
        }
        if (event.getAnswer() != null) {
            group.selectToggle(group.getToggles().get(event.getAnswer()));
        } else {
            send_answer.disableProperty().bind(group.selectedToggleProperty().isNull());
            add(send_answer, 0, answers.size() + 1);
        }
    }
}
