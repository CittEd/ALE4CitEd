package it.cnr.istc.lecture;

import android.content.Context;
import android.support.annotation.NonNull;
import android.util.Log;

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

import it.cnr.istc.lecture.api.Credentials;
import it.cnr.istc.lecture.api.InitResponse;
import it.cnr.istc.lecture.api.LECTurEResource;
import it.cnr.istc.lecture.api.messages.Event;
import it.cnr.istc.lecture.api.messages.HideEvent;
import it.cnr.istc.lecture.api.messages.LostLesson;
import it.cnr.istc.lecture.api.messages.LostStudent;
import it.cnr.istc.lecture.api.messages.Message;
import it.cnr.istc.lecture.api.messages.NewLesson;
import it.cnr.istc.lecture.api.messages.NewStudent;
import it.cnr.istc.lecture.api.messages.RemoveToken;
import it.cnr.istc.lecture.api.messages.Token;
import it.cnr.istc.lecture.api.messages.TokenUpdate;
import retrofit2.Response;
import retrofit2.Retrofit;
import retrofit2.converter.gson.GsonConverterFactory;

public class LECTurEContext {

    private static final String TAG = "LECTurEContext";
    private static final Gson GSON = new Gson();
    private static LECTurEContext instance;
    private LECTurEResource resource;
    private MqttClient mqtt;
    private LECTurEContext() {
        Retrofit retrofit = new Retrofit.Builder().baseUrl("http://" + BuildConfig.LECTURE_HOST + ":" + BuildConfig.SERVICE_PORT).addConverterFactory(GsonConverterFactory.create()).build();
        resource = retrofit.create(LECTurEResource.class);
    }

    public static LECTurEContext getInstance() {
        if (instance == null) instance = new LECTurEContext();
        return instance;
    }

    public boolean login(@NonNull final Context ctx, @NonNull final String email, @NonNull final String password) {
        try {
            Response<InitResponse> response = resource.login(new Credentials(email, password)).execute();
            if (response.isSuccessful()) {
                Log.i(TAG, "Login successful..");
                InitResponse init = response.body();

                try {
                    mqtt = new MqttClient("tcp://" + BuildConfig.LECTURE_HOST + ":" + BuildConfig.MQTT_PORT, String.valueOf(init.user.id), new MemoryPersistence());
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
                    mqtt.subscribe(init.user.id + "/input", new IMqttMessageListener() {
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
                } catch (MqttException e) {
                    Log.w(TAG, "MQTT Connection failed..", e);
                    return false;
                }

                return true;
            } else
                return false;
        } catch (IOException e) {
            Log.w(TAG, "Login failed..", e);
            return false;
        }
    }
}
