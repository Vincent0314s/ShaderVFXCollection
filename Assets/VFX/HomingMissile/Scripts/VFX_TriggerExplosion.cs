using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class VFX_TriggerExplosion : MonoBehaviour
{
    public GameObject PF_Explosion;
    private void OnCollisionEnter(Collision other) {
           if(other.gameObject.tag.Equals("Player")){
            CreateVFX();
        }
    }

    private void CreateVFX(){
        GameObject vfx_Explosion = Instantiate(PF_Explosion,transform.forward,Quaternion.identity);
        Destroy(vfx_Explosion,5f);
    }
}
