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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.Stack;
import java.util.function.BooleanSupplier;
import javafx.beans.property.BooleanProperty;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Rectangle2D;
import javafx.scene.Node;
import javafx.scene.control.Button;
import javafx.scene.control.ButtonBar;
import javafx.scene.control.ButtonType;
import javafx.scene.control.Dialog;
import javafx.scene.control.DialogPane;
import javafx.scene.layout.Pane;
import javafx.stage.Screen;
import javafx.stage.Window;

/**
 *
 * @author Riccardo De Benedictis
 */
public class Wizard<T> extends Dialog<T> {

    private final Stack<WizardPane> pageHistory = new Stack<>();
    private Optional<WizardPane> currentPage = Optional.empty();
    private final BooleanProperty invalid = new SimpleBooleanProperty(false);
    private final ButtonType BUTTON_PREVIOUS = new ButtonType(Context.LANGUAGE.getString("PREVIOUS"), ButtonBar.ButtonData.BACK_PREVIOUS);
    private final EventHandler<ActionEvent> BUTTON_PREVIOUS_ACTION_HANDLER = actionEvent -> {
        actionEvent.consume();
        currentPage = Optional.ofNullable(pageHistory.isEmpty() ? null : pageHistory.pop());
        updatePage();
    };
    private final ButtonType BUTTON_NEXT = new ButtonType(Context.LANGUAGE.getString("NEXT"), ButtonBar.ButtonData.NEXT_FORWARD);
    private final EventHandler<ActionEvent> BUTTON_NEXT_ACTION_HANDLER = actionEvent -> {
        actionEvent.consume();
        currentPage.ifPresent(page -> pageHistory.push(page));
        currentPage = getFlow().advance(currentPage.orElse(null));
        updatePage();
    };
    private final ObjectProperty<Flow> flow = new SimpleObjectProperty<>();

    public Wizard(String title) {
        setTitle(title);

        invalid.addListener((o, ov, nv) -> validateActionState());
    }

    public final Flow getFlow() {
        return flow.get();
    }

    public final void setFlow(Flow flow) {
        this.flow.set(flow);
    }

    public ObjectProperty<Flow> flowProperty() {
        return flow;
    }

    public final void setInvalid(boolean invalid) {
        this.invalid.set(invalid);
    }

    public final boolean isInvalid() {
        return invalid.get();
    }

    public final BooleanProperty invalidProperty() {
        return invalid;
    }

    private void updatePage() {
        if (flow.isNull().get()) {
            return;
        }

        Optional<WizardPane> prevPage = Optional.ofNullable(pageHistory.isEmpty() ? null : pageHistory.peek());
        prevPage.ifPresent(page -> page.onExitingPage(this));

        currentPage.ifPresent(page -> {
            // put in default actions
            List<ButtonType> buttons = page.getButtonTypes();
            if (!buttons.contains(BUTTON_PREVIOUS)) {
                buttons.add(BUTTON_PREVIOUS);
                Button button = (Button) page.lookupButton(BUTTON_PREVIOUS);
                button.addEventFilter(ActionEvent.ACTION, BUTTON_PREVIOUS_ACTION_HANDLER);
            }
            if (!buttons.contains(BUTTON_NEXT)) {
                buttons.add(BUTTON_NEXT);
                Button button = (Button) page.lookupButton(BUTTON_NEXT);
                button.addEventFilter(ActionEvent.ACTION, BUTTON_NEXT_ACTION_HANDLER);
            }
            if (!buttons.contains(ButtonType.FINISH)) {
                buttons.add(ButtonType.FINISH);
            }
            if (!buttons.contains(ButtonType.CANCEL)) {
                buttons.add(ButtonType.CANCEL);
            }

            // then give user a chance to modify the default actions
            page.onEnteringPage(this);

            // Remove from DecorationPane which has been created by e.g. validation
            if (page.getParent() != null && page.getParent() instanceof Pane) {
                Pane parentOfCurrentPage = (Pane) page.getParent();
                parentOfCurrentPage.getChildren().remove(page);
            }

            // Get current position and size
            double previousX = getX();
            double previousY = getY();
            double previousWidth = getWidth();
            double previousHeight = getHeight();
            // and then switch to the new pane
            setDialogPane(page);
            // Resize Wizard to new page
            Window wizard = page.getScene().getWindow();
            wizard.sizeToScene();
            // Center resized Wizard to previous position

            if (!Double.isNaN(previousX) && !Double.isNaN(previousY)) {
                double newWidth = getWidth();
                double newHeight = getHeight();
                int newX = (int) (previousX + (previousWidth / 2.0) - (newWidth / 2.0));
                int newY = (int) (previousY + (previousHeight / 2.0) - (newHeight / 2.0));

                ObservableList<Screen> screens = Screen.getScreensForRectangle(previousX, previousY, 1, 1);
                Screen screen = screens.isEmpty() ? Screen.getPrimary() : screens.get(0);
                Rectangle2D scrBounds = screen.getBounds();
                int minX = (int) Math.round(scrBounds.getMinX());
                int maxX = (int) Math.round(scrBounds.getMaxX());
                int minY = (int) Math.round(scrBounds.getMinY());
                int maxY = (int) Math.round(scrBounds.getMaxY());
                if (newX + newWidth > maxX) {
                    newX = maxX - (int) Math.round(newWidth);
                }
                if (newY + newHeight > maxY) {
                    newY = maxY - (int) Math.round(newHeight);
                }
                if (newX < minX) {
                    newX = minX;
                }
                if (newY < minY) {
                    newY = minY;
                }

                setX(newX);
                setY(newY);
            }
        });

        validateActionState();
    }

    private void validateActionState() {
        final List<ButtonType> currentPaneButtons = getDialogPane().getButtonTypes();

        if (getFlow().canAdvance(currentPage.orElse(null))) {
            currentPaneButtons.remove(ButtonType.FINISH);
        } else {
            currentPaneButtons.remove(BUTTON_NEXT);
        }

        validateButton(BUTTON_PREVIOUS, () -> pageHistory.isEmpty());
        validateButton(BUTTON_NEXT, () -> invalid.get());
        validateButton(ButtonType.FINISH, () -> invalid.get());
    }

    private void validateButton(ButtonType buttonType, BooleanSupplier condition) {
        Button btn = (Button) getDialogPane().lookupButton(buttonType);
        if (btn != null) {
            Node focusOwner = (btn.getScene() != null) ? btn.getScene().getFocusOwner() : null;
            btn.setDisable(condition.getAsBoolean());
            if (focusOwner != null) {
                focusOwner.requestFocus();
            }
        }
    }

    public interface Flow {

        Optional<WizardPane> advance(WizardPane currentPage);

        boolean canAdvance(WizardPane currentPage);
    }

    public static class LinearFlow implements Wizard.Flow {

        private final List<WizardPane> pages;

        public LinearFlow(Collection<WizardPane> pages) {
            this.pages = new ArrayList<>(pages);
        }

        public LinearFlow(WizardPane... pages) {
            this(Arrays.asList(pages));
        }

        @Override
        public Optional<WizardPane> advance(WizardPane currentPage) {
            return Optional.ofNullable(pages.get(pages.indexOf(currentPage) + 1));
        }

        @Override
        public boolean canAdvance(WizardPane currentPage) {
            return pages.indexOf(currentPage) < pages.size() - 1;
        }
    }

    public static class WizardPane extends DialogPane {

        public void onEnteringPage(Wizard wizard) {
        }

        public void onExitingPage(Wizard wizard) {
        }
    }
}
