package pac4j;

import com.fasterxml.jackson.databind.JsonNode;
import com.github.scribejava.core.model.OAuth2AccessToken;
import org.pac4j.core.context.Pac4jConstants;
import org.pac4j.core.exception.HttpAction;
import org.pac4j.core.profile.converter.Converters;
import org.pac4j.oauth.config.OAuth20Configuration;
import org.pac4j.oauth.profile.JsonHelper;
import org.pac4j.oauth.profile.definition.OAuth20ProfileDefinition;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.Arrays;

/**
 * @see <a href="https://discordapp.com/developers/docs/resources/user#user-object">Discord user object</a>
 */
public class DiscordProfileDefinition extends OAuth20ProfileDefinition<DiscordProfile> {

    public static final String DISCRIMINATOR = "discriminator";
    public static final String AVATAR = "avatar";
    public static final String BOT = "bot";
    public static final String MFA_ENABLED = "mfa_enabled";
    public static final String VERIFIED = "verified";

    public DiscordProfileDefinition() {
        super(x -> new DiscordProfile());
        Arrays.asList(new String[] {
            Pac4jConstants.USERNAME, DISCRIMINATOR, AVATAR, EMAIL
        }).forEach(a -> primary(a, Converters.STRING));
        Arrays.asList(new String[] {
            BOT, MFA_ENABLED, VERIFIED
        }).forEach(a -> primary(a, Converters.BOOLEAN));
    }

    @Override
    public String getProfileUrl(OAuth2AccessToken accessToken, OAuth20Configuration configuration) {
        return "https://discordapp.com/api/users/@me";
    }

    @Override
    public DiscordProfile extractUserProfile(String body) throws HttpAction {
        final DiscordProfile profile = newProfile();
        final JsonNode json = JsonHelper.getFirstNode(body);
        if (json != null) {
            String id = (String) JsonHelper.getElement(json, "id");
            profile.setId(id);
            for (final String attribute : getPrimaryAttributes()) {
                convertAndAdd(profile, attribute, JsonHelper.getElement(json, attribute));
            }

            // Get a picture URL image or a fallback image.
            // See <https://discordapp.com/developers/docs/reference#image-formatting-cdn-endpoints>.
            String avatar;
            String discriminator;
            String pictureURL = null;
            if ((avatar = profile.getAttribute(AVATAR, String.class)) != null) {
                pictureURL = String.format("https://cdn.discordapp.com/avatars/%s/%s.png", id, avatar);
            } else if ((discriminator = profile.getAttribute(DISCRIMINATOR, String.class)) != null) {
                int index = Integer.valueOf(discriminator) % 5;
                pictureURL = String.format("https://cdn.discordapp.com/embed/avatars/%d.png", index);
            }
            if (pictureURL != null) {
                try {
                    profile.addAttribute(PICTURE_URL, new URI(pictureURL));
                } catch (URISyntaxException e) {
                    logger.warn("Invalid picture URL!", e);
                }
            }
        }
        return profile;
    }
}
