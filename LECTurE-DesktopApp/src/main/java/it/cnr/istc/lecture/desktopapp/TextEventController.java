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

import it.cnr.istc.lecture.api.messages.TextEvent;
import java.net.URL;
import java.util.ResourceBundle;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.web.WebView;

/**
 *
 * @author Riccardo De Benedictis
 */
public class TextEventController implements Initializable {

    @FXML
    private WebView web_view;
    private final ObjectProperty<TextEvent> event = new SimpleObjectProperty<>();

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        event.addListener((ObservableValue<? extends TextEvent> observable, TextEvent oldValue, TextEvent newValue) -> {
            if (newValue != null) {
                web_view.getEngine().loadContent(newValue.content);
            } else {
                web_view.getEngine().loadContent("");
            }
        });
    }

    public ObjectProperty<TextEvent> eventProperty() {
        return event;
    }
}
