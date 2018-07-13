package it.cnr.istc.lecture;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import it.cnr.istc.lecture.api.Lesson;
import it.cnr.istc.lecture.api.Lesson.LessonState;
import it.cnr.istc.lecture.api.messages.Event;

public class FollowingLessonContext {

    private final Lesson lesson;
    private LessonState state = LessonState.Stopped;
    private long time = 0;
    private final List<Event> events = new ArrayList<>();
    private final List<FollowingLessonListener> listeners = new ArrayList<>();

    FollowingLessonContext(Lesson lesson) {
        this.lesson = lesson;
    }

    public Lesson getLesson() {
        return lesson;
    }

    public LessonState getState() {
        return state;
    }

    public void setState(LessonState state) {
        if (this.state != state) {
            this.state = state;
            for (FollowingLessonListener l : listeners) {
                l.stateChanged(state);
            }
        }
    }

    public long getTime() {
        return time;
    }

    public void setTime(long time) {
        if (this.time != time) {
            this.time = time;
            for (FollowingLessonListener l : listeners) {
                l.timeChanged(time);
            }
        }
    }

    public List<Event> getEvents() {
        return Collections.unmodifiableList(events);
    }

    public void addEvent(final Event e) {
        events.add(e);
        for (FollowingLessonListener l : listeners) {
            l.newEvent(e);
        }
    }

    public void removeEvent(Event e) {
        events.remove(e);
        for (FollowingLessonListener l : listeners) {
            l.removedEvent(e);
        }
    }

    public void addListener(FollowingLessonListener l) {
        listeners.add(l);
    }

    public void removeListener(FollowingLessonListener l) {
        listeners.remove(l);
    }

    public interface FollowingLessonListener {

        void timeChanged(long t);

        void stateChanged(LessonState state);

        void newEvent(Event e);

        void removedEvent(Event e);
    }
}
