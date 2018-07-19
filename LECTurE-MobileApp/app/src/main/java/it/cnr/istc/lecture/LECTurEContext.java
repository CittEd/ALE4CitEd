package it.cnr.istc.lecture;

import android.Manifest;
import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.content.pm.PackageManager;
import android.location.Location;
import android.location.LocationListener;
import android.location.LocationManager;
import android.os.AsyncTask;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.v4.content.ContextCompat;
import android.util.Log;
import android.util.LongSparseArray;

import com.google.gson.Gson;

import org.eclipse.paho.client.mqttv3.IMqttDeliveryToken;
import org.eclipse.paho.client.mqttv3.IMqttMessageListener;
import org.eclipse.paho.client.mqttv3.MqttCallback;
import org.eclipse.paho.client.mqttv3.MqttClient;
import org.eclipse.paho.client.mqttv3.MqttConnectOptions;
import org.eclipse.paho.client.mqttv3.MqttException;
import org.eclipse.paho.client.mqttv3.MqttMessage;
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;

import it.cnr.istc.lecture.api.Credentials;
import it.cnr.istc.lecture.api.InitResponse;
import it.cnr.istc.lecture.api.LECTurEResource;
import it.cnr.istc.lecture.api.Lesson;
import it.cnr.istc.lecture.api.Parameter;
import it.cnr.istc.lecture.api.User;
import it.cnr.istc.lecture.api.messages.Event;
import it.cnr.istc.lecture.api.messages.HideEvent;
import it.cnr.istc.lecture.api.messages.LostLesson;
import it.cnr.istc.lecture.api.messages.LostParameter;
import it.cnr.istc.lecture.api.messages.LostStudent;
import it.cnr.istc.lecture.api.messages.Message;
import it.cnr.istc.lecture.api.messages.NewLesson;
import it.cnr.istc.lecture.api.messages.NewParameter;
import it.cnr.istc.lecture.api.messages.NewStudent;
import it.cnr.istc.lecture.api.messages.RemoveToken;
import it.cnr.istc.lecture.api.messages.Token;
import it.cnr.istc.lecture.api.messages.TokenUpdate;
import it.cnr.istc.lecture.api.model.LessonModel;
import retrofit2.Response;
import retrofit2.Retrofit;
import retrofit2.converter.gson.GsonConverterFactory;

public class LECTurEContext implements LocationListener {

    public static final int ACCESS_FINE_LOCATION_REQUEST_CODE_ASK_PERMISSIONS = 123;
    private static final String TAG = "LECTurEContext";
    private static final Gson GSON = new Gson();
    private static LECTurEContext instance;
    private LECTurEResource resource;
    private MqttClient mqtt;
    /**
     * The current user.
     */
    private User user;
    /**
     * The current user's parameter types.
     */
    private final List<Parameter> par_types = new ArrayList<>();
    private final Map<String, Parameter> id_par_types = new HashMap<>();
    /**
     * The current user's parameter values.
     */
    private final Map<String, Map<String, String>> par_vals = new HashMap<>();
    /**
     * The received events.
     */
    private final List<Event> events = new ArrayList<>();
    /**
     * The lessons followed as a student.
     */
    private final List<FollowingLessonContext> following_lessons = new ArrayList<>();
    private final LongSparseArray<FollowingLessonContext> id_following_lessons = new LongSparseArray<>();
    /**
     * The followed teachers.
     */
    private final List<TeacherContext> teachers = new ArrayList<>();
    private final LongSparseArray<TeacherContext> id_teachers = new LongSparseArray<>();
    /**
     * The lesson models associated to the teacher.
     */
    private final List<LessonModel> models = new ArrayList<>();
    /**
     * The lessons followed as a teacher.
     */
    private final List<TeachingLessonContext> teaching_lessons = new ArrayList<>();
    private final LongSparseArray<TeachingLessonContext> id_teaching_lessons = new LongSparseArray<>();
    /**
     * The following students.
     */
    private final List<StudentContext> students = new ArrayList<>();
    private final LongSparseArray<StudentContext> id_students = new LongSparseArray<>();

    private LECTurEContext() {
        Retrofit retrofit = new Retrofit.Builder().baseUrl("http://" + BuildConfig.LECTURE_HOST + ":" + BuildConfig.SERVICE_PORT).addConverterFactory(GsonConverterFactory.create()).build();
        resource = retrofit.create(LECTurEResource.class);
    }

    public static LECTurEContext getInstance() {
        if (instance == null) instance = new LECTurEContext();
        return instance;
    }

    public User getUser() {
        return user;
    }

    public void setUser(final Context ctx, User user) {
        if (this.user != user) {
            if (this.user != null) {
                // we clear the current data..
                try {
                    par_vals.clear();
                    // a user might become null as a consequence of a connection loss..
                    // we broadcast the lost of a parameter..
                    if (mqtt.isConnected()) for (Parameter par : par_types)
                        mqtt.publish(this.user.id + "/output", GSON.toJson(new LostParameter(par.name)).getBytes(), 1, false);
                    ((Activity) ctx).runOnUiThread(new Runnable() {
                        public void run() {
                            if (ContextCompat.checkSelfPermission(ctx, Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED)
                                ((LocationManager) ctx.getSystemService(Context.LOCATION_SERVICE)).removeUpdates(LECTurEContext.this);
                        }
                    });
                    par_types.clear();
                    id_par_types.clear();
                    events.clear();
                    if (mqtt.isConnected()) for (FollowingLessonContext l_ctx : following_lessons) {
                        // we unsubscribe from the lesson's time and state..
                        mqtt.unsubscribe(this.user.id + "/input/lesson-" + l_ctx.getLesson().id + "/time");
                        mqtt.unsubscribe(this.user.id + "/input/lesson-" + l_ctx.getLesson().id + "/state");
                    }
                    following_lessons.clear();
                    id_following_lessons.clear();
                    teachers.clear();
                    id_teachers.clear();
                    models.clear();
                    // we unsubscribe from the lesson's time and state..
                    for (TeachingLessonContext l_ctx : teaching_lessons)
                        if (mqtt.isConnected()) {
                            mqtt.unsubscribe(this.user.id + "/input/lesson-" + l_ctx.getLesson().id + "/time");
                            mqtt.unsubscribe(this.user.id + "/input/lesson-" + l_ctx.getLesson().id + "/state");
                        }
                    teaching_lessons.clear();
                    id_teaching_lessons.clear();
                    students.clear();
                    id_students.clear();
                    if (mqtt.isConnected()) mqtt.disconnect();
                    mqtt.close();
                } catch (MqttException ex) {
                    Log.e(TAG, null, ex);
                }
            }

            if (user != null) {
                try {
                    mqtt = new MqttClient("tcp://" + BuildConfig.LECTURE_HOST + ":" + BuildConfig.MQTT_PORT, String.valueOf(user.id), new MemoryPersistence());
                    mqtt.setCallback(new MqttCallback() {
                        @Override
                        public void connectionLost(Throwable cause) {
                            Log.e(TAG, "Connection lost..", cause);
                        }

                        @Override
                        public void messageArrived(String topic, MqttMessage message) throws Exception {
                            Log.w(TAG, "Message arrived: " + topic + " - " + message);
                        }

                        @Override
                        public void deliveryComplete(IMqttDeliveryToken token) {
                        }
                    });

                    MqttConnectOptions options = new MqttConnectOptions();
                    options.setCleanSession(true);
                    options.setAutomaticReconnect(true);
                    mqtt.connect(options);
                    Log.i(TAG, "Connected to the MQTT broker..");
                    mqtt.subscribe(user.id + "/input", new IMqttMessageListener() {
                        @Override
                        public void messageArrived(String topic, MqttMessage message) throws Exception {
                            Log.w(TAG, "Message arrived: " + topic + " - " + message);
                            Message m = GSON.fromJson(new String(message.getPayload()), Message.class);
                            switch (m.message_type) {
                                case NewStudent:
                                    // a new student is following this user..
                                    NewStudent new_student = GSON.fromJson(new String(message.getPayload()), NewStudent.class);
                                    break;
                                case LostStudent:
                                    // a student is not following this user anymore..
                                    LostStudent lost_student = GSON.fromJson(new String(message.getPayload()), LostStudent.class);
                                    break;
                                case NewLesson:
                                    // a teacher has created a new lesson for this student..
                                    NewLesson new_lesson = GSON.fromJson(new String(message.getPayload()), NewLesson.class);
                                    break;
                                case LostLesson:
                                    // a teacher has removed a new lesson for this student..
                                    LostLesson lost_lesson = GSON.fromJson(new String(message.getPayload()), LostLesson.class);
                                    break;
                                case Token:
                                    // a new token has been created for a teaching lesson..
                                    Token token = GSON.fromJson(new String(message.getPayload()), Token.class);
                                    break;
                                case TokenUpdate:
                                    // a token of a teaching lesson has been updated..
                                    TokenUpdate token_update = GSON.fromJson(new String(message.getPayload()), TokenUpdate.class);
                                    break;
                                case RemoveToken:
                                    // a token of a teaching lesson has been removed..
                                    RemoveToken remove_token = GSON.fromJson(new String(message.getPayload()), RemoveToken.class);
                                    break;
                                case Event:
                                    // a new event has been created for a following lesson..
                                    Event event = GSON.fromJson(new String(message.getPayload()), Event.class);
                                    break;
                                case HideEvent:
                                    // an event has been removed for a following lesson..
                                    HideEvent hide_event = GSON.fromJson(new String(message.getPayload()), HideEvent.class);
                                    break;
                                case Answer:
                                    break;
                                default:
                                    throw new AssertionError(m.message_type.name());
                            }
                        }
                    });

                    for (Parameter par : user.par_types.values()) {
                        par_types.add(par);
                        // we broadcast the existence of a new parameter..
                        mqtt.publish(user.id + "/output", GSON.toJson(new NewParameter(par)).getBytes(), 1, false);
                    }
                    ((Activity) ctx).runOnUiThread(new Runnable() {
                        public void run() {
                            if (ContextCompat.checkSelfPermission(ctx, Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED)
                                ((LocationManager) ctx.getSystemService(Context.LOCATION_SERVICE)).requestLocationUpdates(LocationManager.GPS_PROVIDER, 0, 0, LECTurEContext.this);
                        }
                    });
                    for (Map.Entry<String, Map<String, String>> par_val : user.par_values.entrySet()) {
                        par_vals.put(par_val.getKey(), par_val.getValue());
                        // we broadcast the the new value of the parameter..
                        mqtt.publish(user.id + "/output/" + par_val.getKey(), GSON.toJson(par_val.getValue()).getBytes(), 1, true);
                    }
                } catch (MqttException e) {
                    Log.w(TAG, "MQTT Connection failed..", e);
                }
            }
            this.user = user;
        }
    }

    public List<Event> getEvents() {
        return Collections.unmodifiableList(events);
    }

    public List<FollowingLessonContext> getFollowingLessons() {
        return Collections.unmodifiableList(following_lessons);
    }

    public void addFollowingLesson(FollowingLessonContext l) {
        following_lessons.add(l);
        id_following_lessons.put(l.getLesson().id, l);
    }

    public void removeFollowingLesson(FollowingLessonContext l) {
        following_lessons.remove(l);
        id_following_lessons.remove(l.getLesson().id);
    }

    public List<TeacherContext> getTeachers() {
        return Collections.unmodifiableList(teachers);
    }

    public void addTeacher(TeacherContext t) {
        teachers.add(t);
        id_teachers.put(t.getTeacher().id, t);
    }

    public void removeTeacher(TeacherContext t) {
        teachers.remove(t);
        id_teachers.remove(t.getTeacher().id);
    }

    public List<LessonModel> getModels() {
        return Collections.unmodifiableList(models);
    }

    public List<TeachingLessonContext> getTeachingLessons() {
        return Collections.unmodifiableList(teaching_lessons);
    }

    public void addTeachingLesson(TeachingLessonContext l) {
        teaching_lessons.add(l);
        id_teaching_lessons.put(l.getLesson().id, l);
    }

    public void removeTeachingLesson(TeachingLessonContext l) {
        teaching_lessons.remove(l);
        id_teaching_lessons.remove(l.getLesson().id);
    }

    public List<StudentContext> getStudents() {
        return Collections.unmodifiableList(students);
    }

    public void addStudent(StudentContext s) {
        students.add(s);
        id_students.put(s.getStudent().id, s);
    }

    public void removeStudent(StudentContext s) {
        students.remove(s);
        id_students.remove(s.getStudent().id);
    }

    @SuppressLint("StaticFieldLeak")
    public boolean login(@NonNull final Context ctx, @NonNull final String email, @NonNull final String password) throws ExecutionException, InterruptedException {
        return new AsyncTask<Credentials, Integer, Boolean>() {
            @Override
            protected Boolean doInBackground(Credentials... credentials) {
                try {
                    Response<InitResponse> response = resource.login(credentials[0]).execute();
                    if (response.isSuccessful()) {
                        Log.i(TAG, "Login successful..");
                        InitResponse init = response.body();

                        // we set the parameters of init's user (these parameters will be communicated to the server..)
                        Map<String, Parameter> c_par_types = new HashMap<>();
                        Map<String, Map<String, String>> c_par_values = new HashMap<>();

                        if (ContextCompat.checkSelfPermission(ctx, Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED) {
                            Parameter gps = new Parameter();
                            gps.name = "GPS";
                            gps.properties = new HashMap<>(2);
                            gps.properties.put("latitude", "numeric");
                            gps.properties.put("longitude", "numeric");
                            c_par_types.put("GPS", gps);

                            Map<String, String> gps_pos = new HashMap<>(2);
                            final Location last_location = ((LocationManager) ctx.getSystemService(Context.LOCATION_SERVICE)).getLastKnownLocation(LocationManager.GPS_PROVIDER);
                            gps_pos.put("latitude", Double.toString(last_location.getLatitude()));
                            gps_pos.put("longitude", Double.toString(last_location.getLongitude()));
                            c_par_values.put("GPS", gps_pos);
                        }

                        init.user.par_types = c_par_types;
                        init.user.par_values = c_par_values;

                        setUser(ctx, init.user);

                        // we add the following lessons..
                        for (Lesson lesson : init.following_lessons)
                            addFollowingLesson(new FollowingLessonContext(lesson));

                        // we add the teachers..
                        for (User teacher : init.teachers)
                            addTeacher(new TeacherContext(teacher));

                        // we add the available models..
                        models.addAll(init.models);

                        // we add the teaching lessons..
                        for (Lesson teaching_lesson : init.teaching_lessons) {
                            LessonModel model = null;
                            for (LessonModel m : init.models) {
                                if (m.id.equals(teaching_lesson.model)) model = m;
                                break;
                            }
                            addTeachingLesson(new TeachingLessonContext(teaching_lesson, model));
                        }

                        // we add the students..
                        for (User student : init.students)
                            addStudent(new StudentContext(student));

                        return true;
                    } else
                        return false;
                } catch (IOException e) {
                    Log.w(TAG, "Login failed..", e);
                    return false;
                }
            }
        }.execute(new Credentials(email, password)).get();
    }

    public void logout(Context ctx) {
        setUser(ctx, null);
    }

    @Override
    public void onLocationChanged(Location location) {
        if (mqtt != null && mqtt.isConnected()) {
            Map<String, String> gps_pos = new HashMap<>(2);
            gps_pos.put("latitude", Double.toString(location.getLatitude()));
            gps_pos.put("longitude", Double.toString(location.getLongitude()));
            try {
                mqtt.publish(user.id + "/output/GPS", GSON.toJson(gps_pos).getBytes(), 1, true);
            } catch (MqttException e) {
                Log.w(TAG, "GPS update MQTT communication failed..", e);
            }
        }
    }

    @Override
    public void onStatusChanged(String provider, int status, Bundle extras) {
    }

    @Override
    public void onProviderEnabled(String provider) {
    }

    @Override
    public void onProviderDisabled(String provider) {
    }
}
