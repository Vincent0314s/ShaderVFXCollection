using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Missile : MonoBehaviour
{
    public Transform target;
    public float turnSpeed = 1f;
    public float moveSpeed = 10f;
    private Rigidbody rb;
    private Transform m_Transform;
    private MeshRenderer[] meshRenderer;
    private BoxCollider boxCollider;
    private bool isAlive;

    private void Awake() {
        rb = GetComponent<Rigidbody>();
        m_Transform = transform;
        meshRenderer = GetComponentsInChildren<MeshRenderer>();
        boxCollider = GetComponent<BoxCollider>();
    }

    void Start()
    {
        isAlive = true;
        if(target == null)
            target = GameObject.FindGameObjectWithTag("Player").transform;
                
    }

    private void FixedUpdate() {
         if(rb == null){
            Debug.LogError("Rigidbody is null");
            return;
        }   
        if(isAlive){
            rb.velocity = m_Transform.forward * moveSpeed;

            var lookAtTarget = Quaternion.LookRotation(target.position - m_Transform.position);
            rb.MoveRotation(Quaternion.RotateTowards(m_Transform.rotation, lookAtTarget,turnSpeed));
        }
       
    }

    private void OnCollisionEnter(Collision other) {
        if(other.gameObject.tag.Equals("Player")){
            foreach (var mesh in meshRenderer)
            {
                mesh.enabled = false;
            }
            boxCollider.enabled = false;
            isAlive = false;
            rb.isKinematic = true;
            Destroy(gameObject,0.75f);
        }
    }
}
