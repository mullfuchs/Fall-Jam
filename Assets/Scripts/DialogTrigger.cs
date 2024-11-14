using UnityEngine;

public class DialogTrigger
{
    private void OnTriggerEnter(Collider other)
    {
        if(other.tag == "Protagonist")
        {
            Debug.Log("Dialog Trigger entered");
        }
    }
}
