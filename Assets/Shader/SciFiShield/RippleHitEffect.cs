using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class RippleHitEffect : MonoBehaviour
{
    public GameObject VFX_ripple;
    private Material mat;

    private void OnCollisionEnter(Collision collision)
    {
        if (collision.gameObject.tag.Equals("Bullet")) {
            var ripple = Instantiate(VFX_ripple,transform) as GameObject;
            var psr = ripple.transform.GetChild(0).GetComponent<ParticleSystemRenderer>();
            mat = psr.material;
            mat.SetVector("_SphereCenter", collision.GetContact(0).point);
            Debug.Log("Center " + mat.GetVector("_SphereCenter"));
            Debug.Log("Conttact " + collision.GetContact(0).point);
            Destroy(ripple,2f);
        }
    }
}
